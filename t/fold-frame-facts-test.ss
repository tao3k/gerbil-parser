(import :gerbil-parser/language-test-support)
;;; Concrete lowering controls; actual Org AOT remains a separate gate.
(import :std/test ../src/compiler/event-fold-scheme-statements
        ../src/compiler/event-fold-scheme-context)

(def scan '(line-skip-horizontal start))
(def bindings '((at . at) (ready . ready)))
(def (scan-loops value)
  (cond ((not (pair? value)) 0)
        ((and (eq? (car value) 'let) (pair? (cdr value))
              (eq? (cadr value) 'loop)) 1)
        ((eq? (car value) 'quote) 0)
        (else (+ (scan-loops (car value)) (scan-loops (cdr value))))))
(def (contains? value target)
  (or (equal? value target)
      (and (pair? value)
           (or (contains? (car value) target) (contains? (cdr value) target)))))
(def (lower forms (indices '()))
  (parameterize ((current-fold-scheme-counter 0))
    (fold-scheme-frame-statements forms bindings indices '())))
(deflanguage-parser-tests fold-frame-facts-test "Immutable frame facts and ordered transitions"
  (engine)
    (scenario "multiple offsets share one actual scan"
      (equal (scan-loops (lower `((set-uint at (offset ,scan))
                                 (token Text ,scan (line-step ,scan)))))  1)
      (parameters ((current-fold-scheme-tape-kinds '((Text 0))))
        (scope ((code (lower '((set-uint at (offset (line-scan-until start "]")))
                            (set-bool ready (bool #t))
                            (token Text start (line-scan-until start "]"))))))
          (equal (scan-loops code)  1)
          (equal (contains? code '(set! ready #t))  #t))))
    (property "single and state-dependent scans are not hoisted"
      (bindings)
      (equal (scan-loops (lower `((token Text ,scan end))))  1)
      (equal (scan-loops (lower '((set-uint at (offset (line-skip-horizontal (state-offset at))))
                                  (token Text (line-skip-horizontal (state-offset at)) end))))  2))
    (property "nested frames cannot inherit an outer fact"
      (bindings)
      (equal (scan-loops
        (lower `((with-source-bounds start end
                   ((set-uint at (offset ,scan)) (token Text ,scan end))))
               '((%frame-horizontal . forbidden-outer-value))))  1))
    (property "state writes and event order remain in the frame"
      (bindings (code (lower `((set-uint at (offset ,scan))
                          (set-bool ready (bool #t)) (start-node Text)
                          (token Text ,scan end) (finish-node)))))
      (equal (contains? code '(set! ready #t))  #t)
      (equal (contains? code '(set! events (cons '(start Text) events)))  #t)
      (equal (contains? code '(set! events (cons '(finish) events)))  #t))
    (property "conditional queries retain short circuit evaluation"
      (bindings (code (lower `((if (and (state ready)
                                  (future-heading-title? "*" " " 1 "END"))
                              ((token Text ,scan end)) ())
                          (token Text ,scan end)))))
      (equal (scan-loops code)  1)
      (equal (contains? code 'ready)  #t)))
