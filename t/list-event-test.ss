(import :gerbil-parser/language-test-support)
;;; Non-Org declaration controls for scoped native list structure and spans.
(import :std/test
        (only-in :gerbil-parser/src/modules/parser/interface
                 make-list-line make-text-line make-source-event-scope
                 source-list-initial source-list-scan-form source-list-present-condition
                 source-list-ordered-condition source-list-active-condition
                 source-list-first-blank-condition source-list-continuation-condition
                 source-list-content-offset source-list-item-open-forms
                 source-list-paragraph-span-forms source-list-paragraph-close-form
                 source-list-close-forms source-list-first-blank-forms source-list-reset-blank-form)
        (only-in :gerbil-parser/src/compiler/event-fold-runtime run-event-fold))

(def rule (make-list-line "+" #t 'Sequence 'Entry 'Marker 'Space 4))
(def text (make-text-line 'Blank 'Raw #f 'Content))
(def scope (make-source-event-scope 'non-org-list))
(def (starts events node)
  (length (filter (lambda (event) (equal? event (list 'start node))) events)))
(def (covers? events source)
  (let loop ((rest events) (offset 0) (depth 0))
    (if (null? rest)
      (and (= offset (u8vector-length (string->utf8 source))) (= depth 0))
      (let (event (car rest))
        (case (car event)
          ((token) (and (= offset (caddr event)) (<= offset (cadddr event))
                        (loop (cdr rest) (cadddr event) depth)))
          ((start) (loop (cdr rest) offset (+ depth 1)))
          ((finish) (and (> depth 0) (loop (cdr rest) offset (- depth 1)))))))))
(def (events source (span 'marker) (declaration rule))
  (def close (source-list-close-forms text scope))
  (def paragraph-close (source-list-paragraph-close-form text scope))
  (def (item ordered?)
    `(,paragraph-close
      ,@(source-list-item-open-forms declaration scope ordered? bullet-span: span)
      ,@(source-list-paragraph-span-forms text scope (source-list-content-offset scope) 'end)
      ,(source-list-reset-blank-form scope)))
  (run-event-fold source 'Document (source-list-initial declaration scope text)
    `(,(source-list-scan-form declaration scope)
      (if ,(source-list-present-condition scope)
          ((if ,(source-list-ordered-condition scope) ,(item #t) ,(item #f)))
          ((if ,(source-list-active-condition scope)
               ((if (line-blank?)
                    ((if ,(source-list-first-blank-condition scope)
                         ,(source-list-first-blank-forms declaration text scope)
                         (,@close (token Raw start end))))
                    ((if ,(source-list-continuation-condition declaration scope)
                         (,@(source-list-paragraph-span-forms text scope 'start 'end)
                          ,(source-list-reset-blank-form scope))
                         (,@close (token Raw start end))))))
               ((token Raw start end))))))
    (source-list-close-forms text scope #f '() #f)))
(deflanguage-parser-tests list-event-test "native declarative list transitions"
  (engine)
    (property "non-Org siblings preserve Unicode CRLF and EOF"
      (bindings (source "+ α\r\n+ β") (result (events source)))
      (equal (starts result 'Sequence)  1)
      (equal (starts result 'Entry)  2)
      (equal (covers? result source)  #t))
    (property "nested dedent and sibling transitions balance every node"
      (bindings (source "+ a\n  + b\n    + c\n  + d\n+ e\n") (result (events source)))
      (equal (starts result 'Sequence)  3)
      (equal (starts result 'Entry)  5)
      (equal (covers? result source)  #t))
    (property "ordered kind changes replace a same-column container"
      (bindings (source "+ a\n1. b\n+ c") (result (events source)))
      (equal (starts result 'Sequence)  3)
      (equal (covers? result source)  #t))
    (property "tab width belongs to the admitted list declaration"
      (bindings (source "+ a\n\t+ b\n   + c\n") (result (events source)))
      (equal (starts result 'Sequence)  3)
      (equal (covers? result source)  #t))
    (scenario "one blank retains the item and two blanks terminate it"
      (each (source) ('("+ a\n\n  b\n+ c" "+ a\n\n\nplain\n"))
                  (equal (covers? (events source) source)  #t))
      (equal (starts (events "+ a\n\n  b\n+ c") 'Sequence)  1))
    (property "bullet span is explicit language policy"
      (bindings)
      (equal (and (member '(token Marker 0 1) (events "+ a")) #t)  #t)
      (equal (and (member '(token Marker 0 2) (events "+ a" 'with-separator)) #t)  #t)
      (equal (covers? (events "+ a" 'with-separator) "+ a")  #t))
    (scenario "disabled ordered markers and empty documents stay lossless"
      (scope ((unordered (make-list-line "+" #f 'Sequence 'Entry 'Marker 'Space 4)))
        (equal (starts (events "1. raw\n+ item" 'marker unordered) 'Sequence)  1)
        (equal (covers? (events "1. raw\n+ item" 'marker unordered) "1. raw\n+ item")  #t))
      (equal (covers? (events "") "")  #t))
    (scenario "invalid declarations and transition policies fail before execution"
      (raises (source-list-initial #f scope text) true)
      (raises (source-list-initial rule #f text) true)
      (raises (source-list-item-open-forms rule scope 'ordered) true)
      (raises (source-list-item-open-forms rule scope #f bullet-span: 'unknown) true)))
