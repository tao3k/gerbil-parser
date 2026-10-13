;;; Concrete lowering controls; actual Org AOT remains a separate gate.
(import :std/test ../src/compiler/event-fold-scheme-statements
        ../src/compiler/event-fold-scheme-context)
(export fold-frame-facts-test)
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
(def fold-frame-facts-test
  (test-suite "Immutable frame facts and ordered transitions"
    (test-case "multiple offsets share one actual scan"
      (check (scan-loops (lower `((set-uint at (offset ,scan))
                                 (token Text ,scan (line-step ,scan))))) => 1)
      (parameterize ((current-fold-scheme-tape-kinds '((Text 0))))
        (let (code (lower '((set-uint at (offset (line-scan-until start "]")))
                            (set-bool ready (bool #t))
                            (token Text start (line-scan-until start "]")))))
          (check (scan-loops code) => 1)
          (check (contains? code '(set! ready #t)) => #t))))
    (test-case "single and state-dependent scans are not hoisted"
      (check (scan-loops (lower `((token Text ,scan end)))) => 1)
      (check (scan-loops (lower '((set-uint at (offset (line-skip-horizontal (state-offset at))))
                                  (token Text (line-skip-horizontal (state-offset at)) end)))) => 2))
    (test-case "nested frames cannot inherit an outer fact"
      (check (scan-loops
        (lower `((with-source-bounds start end
                   ((set-uint at (offset ,scan)) (token Text ,scan end))))
               '((%frame-horizontal . forbidden-outer-value)))) => 1))
    (test-case "state writes and event order remain in the frame"
      (let (code (lower `((set-uint at (offset ,scan))
                          (set-bool ready (bool #t)) (start-node Text)
                          (token Text ,scan end) (finish-node))))
        (check (contains? code '(set! ready #t)) => #t)
        (check (contains? code '(set! events (cons '(start Text) events))) => #t)
        (check (contains? code '(set! events (cons '(finish) events))) => #t)))
    (test-case "conditional queries retain short circuit evaluation"
      (let (code (lower `((if (and (state ready)
                                  (future-heading-title? "*" " " 1 "END"))
                              ((token Text ,scan end)) ())
                          (token Text ,scan end))))
        (check (scan-loops code) => 1)
        (check (contains? code 'ready) => #t)))))
