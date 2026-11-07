(import :gerbil-parser/language-test-support)
;;; Distinct rule instances share a native request without private-state aliases.
(import :std/test
        (only-in :gerbil-parser/src/declarations
                 make-source-event-scope source-event-name make-text-line
                 make-table-line make-inline-link source-table-row-initial
                 source-table-row-forms source-paragraph-initial
                 source-paragraph-span-forms source-paragraph-close-form
                 source-inline-link-initial source-inline-link-scan-forms)
        (only-in :gerbil-parser/src/compiler/event-fold-runtime run-event-fold rust-state-name))

(def a (make-source-event-scope 'first))
(def b (make-source-event-scope 'second))
(def table (make-table-line ";" 'Table 'Row 'Rule 'Cell 'Separator 'Value 'Trivia 'RuleText))
(def paragraph (make-text-line 'Blank 'Text #f 'Paragraph))
(def (has? events event) (and (member event events) #t))
(deflanguage-parser-tests source-event-scope-test "native rule-instance state ownership"
  (engine)
    (property "allocation is deterministic and delimiter-safe"
      (bindings)
      (equal (source-event-name a 'open)  (source-event-name (make-source-event-scope 'first) 'open))
      (equal (eq? (source-event-name a 'open) (source-event-name b 'open))  #f)
      (equal (eq? (source-event-name (make-source-event-scope 'a:b) 'c)
                  (source-event-name (make-source-event-scope 'a) 'b:c))  #f)
      (equal (equal? (rust-state-name (source-event-name (make-source-event-scope 'a-b) 'open))
                     (rust-state-name (source-event-name (make-source-event-scope 'a_b) 'open)))  #f))
    (property "two paragraph instances retain separate spans in one frame"
      (bindings (events
            (run-event-fold "α\nβ" 'Document
              (append (source-paragraph-initial paragraph a)
                      (source-paragraph-initial paragraph b))
              `((if (uint-equal? (offset start) (uint 0))
                    ,(source-paragraph-span-forms paragraph a 'start 'end)
                    ,(source-paragraph-span-forms paragraph b 'start 'end)))
              (list (source-paragraph-close-form paragraph b)
                    (source-paragraph-close-form paragraph a)))))
      (equal (has? events '(token Text 0 3))  #t)
      (equal (has? events '(token Text 3 5))  #t)
      (equal (length (filter (lambda (event) (equal? event '(start Paragraph))) events))  2))
    (scenario "table instance does not read or reset another instance's state"
      (scope ((events
            (run-event-fold ";a;b;" 'Document
              (append (source-table-row-initial table a) (source-table-row-initial table b))
              `((set-bool ,(source-event-name a 'table-escaped) (bool #t))
                ,@(source-table-row-forms table b)
                (if (state ,(source-event-name a 'table-escaped))
                    ((start-node Preserved) (finish-node))
                    ((start-node Lost) (finish-node))))
              '())))
        (equal (has? events '(start Preserved))  #t)
        (equal (length (filter (lambda (event) (equal? event '(start Cell))) events))  2)))
    (property "truncated first link cannot capture a second rule's target"
      (bindings (first (make-text-line 'Text 'Raw
                     (make-inline-link "<<" "::" ">>" 'First 'FirstTarget 'Label 'Boundary))) (second (make-text-line 'Text 'Raw
                      (make-inline-link "<{" "|" "}>" 'Second 'SecondTarget 'Label 'Boundary))) (events
              (run-event-fold "<<broken <{ok}>" 'Document
                (append (source-inline-link-initial first a)
                        (source-inline-link-initial second b) '((scan-cursor 0)))
                `((for-line-bytes scan-index start (line-content-end)
                    ,(append (source-inline-link-scan-forms first a index: 'scan-index cursor: 'scan-cursor)
                             (source-inline-link-scan-forms second b index: 'scan-index cursor: 'scan-cursor)))
                  (token Raw (state-offset scan-cursor) end)) '())))
      (equal (has? events '(token SecondTarget 11 13))  #t)
      (equal (has? events '(start First))  #f))
    (scenario "external helper parameter may match a private role name"
      (scope ((events
            (run-event-fold ";x;" 'Document
              (append (source-table-row-initial table a) '((table-escaped 7)))
              (source-table-row-forms table a 'content '(table-escaped)) '()
              '((content ((table-escaped 0))
                 ((if (uint-equal? (state table-escaped) (uint 7))
                      ((token Value start end)) ((token Wrong start end))))
                 (table-escaped))))))
        (equal (has? events '(token Value 1 2))  #t)))
    (scenario "invalid scopes are rejected at projection boundaries"
      (raises (make-source-event-scope "first") true)
      (raises (source-event-name #f 'open) true)
      (raises (source-event-name a "open") true)
      (raises (source-table-row-initial table #f) true)
      (raises (source-paragraph-initial paragraph #f) true)
      (raises (source-inline-link-initial
                         (make-text-line 'Text 'Raw
                           (make-inline-link "<<" "::" ">>" 'Link 'Target 'Label 'Boundary)) #f) true)))
