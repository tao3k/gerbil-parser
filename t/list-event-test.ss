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
(export list-event-test)
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
(def list-event-test
  (test-suite "native declarative list transitions"
    (test-case "non-Org siblings preserve Unicode CRLF and EOF"
      (let* ((source "+ α\r\n+ β") (result (events source)))
        (check (starts result 'Sequence) => 1)
        (check (starts result 'Entry) => 2)
        (check (covers? result source) => #t)))
    (test-case "nested dedent and sibling transitions balance every node"
      (let* ((source "+ a\n  + b\n    + c\n  + d\n+ e\n")
             (result (events source)))
        (check (starts result 'Sequence) => 3)
        (check (starts result 'Entry) => 5)
        (check (covers? result source) => #t)))
    (test-case "ordered kind changes replace a same-column container"
      (let* ((source "+ a\n1. b\n+ c") (result (events source)))
        (check (starts result 'Sequence) => 3)
        (check (covers? result source) => #t)))
    (test-case "tab width belongs to the admitted list declaration"
      (let* ((source "+ a\n\t+ b\n   + c\n") (result (events source)))
        (check (starts result 'Sequence) => 3)
        (check (covers? result source) => #t)))
    (test-case "one blank retains the item and two blanks terminate it"
      (for-each (lambda (source)
                  (check (covers? (events source) source) => #t))
                '("+ a\n\n  b\n+ c" "+ a\n\n\nplain\n"))
      (check (starts (events "+ a\n\n  b\n+ c") 'Sequence) => 1))
    (test-case "bullet span is explicit language policy"
      (check (and (member '(token Marker 0 1) (events "+ a")) #t) => #t)
      (check (and (member '(token Marker 0 2) (events "+ a" 'with-separator)) #t) => #t)
      (check (covers? (events "+ a" 'with-separator) "+ a") => #t))
    (test-case "disabled ordered markers and empty documents stay lossless"
      (let (unordered (make-list-line "+" #f 'Sequence 'Entry 'Marker 'Space 4))
        (check (starts (events "1. raw\n+ item" 'marker unordered) 'Sequence) => 1)
        (check (covers? (events "1. raw\n+ item" 'marker unordered) "1. raw\n+ item") => #t))
      (check (covers? (events "") "") => #t))
    (test-case "invalid declarations and transition policies fail before execution"
      (check-exception (source-list-initial #f scope text) true)
      (check-exception (source-list-initial rule #f text) true)
      (check-exception (source-list-item-open-forms rule scope 'ordered) true)
      (check-exception (source-list-item-open-forms rule scope #f bullet-span: 'unknown) true))))
