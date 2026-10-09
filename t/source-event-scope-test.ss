;;; Distinct rule instances share a native request without private-state aliases.
(import :std/test
        (only-in :gerbil-parser/src/modules/parser/interface
                 make-source-event-scope source-event-name make-text-line
                 make-table-line make-inline-link source-table-row-initial
                 source-table-row-forms source-paragraph-initial
                 source-paragraph-span-forms source-paragraph-close-form
                 source-inline-link-initial source-inline-link-scan-forms)
        (only-in :gerbil-parser/src/compiler/event-fold-runtime run-event-fold rust-state-name))
(export source-event-scope-test)
(def a (make-source-event-scope 'first))
(def b (make-source-event-scope 'second))
(def table (make-table-line ";" 'Table 'Row 'Rule 'Cell 'Separator 'Value 'Trivia 'RuleText))
(def paragraph (make-text-line 'Blank 'Text #f 'Paragraph))
(def (has? events event) (and (member event events) #t))
(def source-event-scope-test
  (test-suite "native rule-instance state ownership"
    (test-case "allocation is deterministic and delimiter-safe"
      (check (source-event-name a 'open) => (source-event-name (make-source-event-scope 'first) 'open))
      (check (eq? (source-event-name a 'open) (source-event-name b 'open)) => #f)
      (check (eq? (source-event-name (make-source-event-scope 'a:b) 'c)
                  (source-event-name (make-source-event-scope 'a) 'b:c)) => #f)
      (check (equal? (rust-state-name (source-event-name (make-source-event-scope 'a-b) 'open))
                     (rust-state-name (source-event-name (make-source-event-scope 'a_b) 'open))) => #f))
    (test-case "two paragraph instances retain separate spans in one frame"
      (let (events
            (run-event-fold "α\nβ" 'Document
              (append (source-paragraph-initial paragraph a)
                      (source-paragraph-initial paragraph b))
              `((if (uint-equal? (offset start) (uint 0))
                    ,(source-paragraph-span-forms paragraph a 'start 'end)
                    ,(source-paragraph-span-forms paragraph b 'start 'end)))
              (list (source-paragraph-close-form paragraph b)
                    (source-paragraph-close-form paragraph a))))
        (check (has? events '(token Text 0 3)) => #t)
        (check (has? events '(token Text 3 5)) => #t)
        (check (length (filter (lambda (event) (equal? event '(start Paragraph))) events)) => 2)))
    (test-case "table instance does not read or reset another instance's state"
      (let (events
            (run-event-fold ";a;b;" 'Document
              (append (source-table-row-initial table a) (source-table-row-initial table b))
              `((set-bool ,(source-event-name a 'table-escaped) (bool #t))
                ,@(source-table-row-forms table b)
                (if (state ,(source-event-name a 'table-escaped))
                    ((start-node Preserved) (finish-node))
                    ((start-node Lost) (finish-node))))
              '()))
        (check (has? events '(start Preserved)) => #t)
        (check (length (filter (lambda (event) (equal? event '(start Cell))) events)) => 2)))
    (test-case "truncated first link cannot capture a second rule's target"
      (let* ((first (make-text-line 'Text 'Raw
                     (make-inline-link "<<" "::" ">>" 'First 'FirstTarget 'Label 'Boundary)))
             (second (make-text-line 'Text 'Raw
                      (make-inline-link "<{" "|" "}>" 'Second 'SecondTarget 'Label 'Boundary)))
             (events
              (run-event-fold "<<broken <{ok}>" 'Document
                (append (source-inline-link-initial first a)
                        (source-inline-link-initial second b) '((scan-cursor 0)))
                `((for-line-bytes scan-index start (line-content-end)
                    ,(append (source-inline-link-scan-forms first a index: 'scan-index cursor: 'scan-cursor)
                             (source-inline-link-scan-forms second b index: 'scan-index cursor: 'scan-cursor)))
                  (token Raw (state-offset scan-cursor) end)) '())))
        (check (has? events '(token SecondTarget 11 13)) => #t)
        (check (has? events '(start First)) => #f)))
    (test-case "external helper parameter may match a private role name"
      (let (events
            (run-event-fold ";x;" 'Document
              (append (source-table-row-initial table a) '((table-escaped 7)))
              (source-table-row-forms table a 'content '(table-escaped)) '()
              '((content ((table-escaped 0))
                 ((if (uint-equal? (state table-escaped) (uint 7))
                      ((token Value start end)) ((token Wrong start end))))
                 (table-escaped)))))
        (check (has? events '(token Value 1 2)) => #t)))
    (test-case "invalid scopes are rejected at projection boundaries"
      (check-exception (make-source-event-scope "first") true)
      (check-exception (source-event-name #f 'open) true)
      (check-exception (source-event-name a "open") true)
      (check-exception (source-table-row-initial table #f) true)
      (check-exception (source-paragraph-initial paragraph #f) true)
      (check-exception (source-inline-link-initial
                         (make-text-line 'Text 'Raw
                           (make-inline-link "<<" "::" ">>" 'Link 'Target 'Label 'Boundary)) #f) true))))
