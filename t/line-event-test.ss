;;; Native execution of non-Org declarations through the public engine surface.
(import :std/test
        (only-in :gerbil-parser/src/modules/parser/interface
                 make-table-line make-text-line source-table-row-initial
                 source-table-row-forms source-paragraph-initial
                 source-paragraph-close-form source-paragraph-line-form)
        (only-in :gerbil-parser/src/compiler/event-fold-runtime run-event-fold))
(export line-event-test)
(def table (make-table-line ";" 'Table 'Row 'RuleRow 'Cell
                           'Separator 'Value 'Trivia 'RuleText))
(def text (make-text-line 'Blank 'Text #f 'Paragraph))
(def (starts events node)
  (length (filter (lambda (event) (equal? event (list 'start node))) events)))
(def (source-covered? events source)
  (let loop ((rest events) (offset 0))
    (if (null? rest) (= offset (u8vector-length (string->utf8 source)))
      (let (event (car rest))
        (if (eq? (car event) 'token)
          (and (= (caddr event) offset) (<= offset (cadddr event))
               (loop (cdr rest) (cadddr event)))
          (loop (cdr rest) offset))))))
(def (table-events source (escape 33))
  (run-event-fold source 'Document (source-table-row-initial table)
                 (source-table-row-forms table escape-byte: escape
                                         rule-bytes: '(61 9 32) rule-marker: 61)
                 '()))
(def (paragraph-events source)
  (run-event-fold source 'Document (source-paragraph-initial text)
                 (list (source-paragraph-line-form text))
                 (list (source-paragraph-close-form text #f '() #f))))
(def line-event-test
  (test-suite "native declarative line events"
    (test-case "non-Org delimiter, escape, empty cell and Unicode source"
      (let* ((source ";λ!;b;;z;\r\n") (events (table-events source)))
        (check (starts events 'Cell) => 3)
        (check (source-covered? events source) => #t)))
    (test-case "escape parity and no-escape policy are declaration driven"
      (check (starts (table-events ";a!!;b;") 'Cell) => 2)
      (check (starts (table-events ";a!;b;" #f) 'Cell) => 2)
      (check (starts (table-events ";a!;b;") 'Cell) => 1))
    (test-case "rule-row marker is language policy, not hardcoded Org bytes"
      (check (starts (table-events ";===;\n") 'RuleRow) => 1)
      (check (starts (table-events ";---;\n") 'Row) => 1)
      (check (source-covered? (table-events ";===;\n") ";===;\n") => #t))
    (test-case "consecutive rows reset escape and separator state"
      (let* ((source ";a!\n;b;\n") (events (table-events source)))
        (check (starts events 'Row) => 2)
        (check (starts events 'Cell) => 2)
        (check (source-covered? events source) => #t)))
    (test-case "paragraph lifetime handles CRLF, blank lines and EOF"
      (let* ((source "α\r\nβ\n\nγ") (events (paragraph-events source)))
        (check (starts events 'Paragraph) => 2)
        (check (starts events 'Blank) => 1)
        (check (source-covered? events source) => #t)))
    (test-case "empty source and leading/trailing blank lines stay lossless"
      (for-each
       (lambda (source)
         (check (source-covered? (paragraph-events source) source) => #t))
       '("" "\n\n" "\nα\n\n" "α")))
    (test-case "content helpers receive explicit named state parameters"
      (let* ((forms (source-table-row-forms table 'content '(policy)))
             (events (run-event-fold ";x;" 'Document
                        (append (source-table-row-initial table) '((policy 7)))
                        forms '()
                        '((content ((policy 0))
                           ((if (uint-equal? (state policy) (uint 7))
                                ((token Value start end))
                                ((token Wrong start end)))) (policy))))))
        (check (and (member '(token Value 1 2) events) #t) => #t)))
    (test-case "invalid declarations and bindings are rejected before execution"
      (check-exception (source-table-row-forms #f) true)
      (check-exception (source-paragraph-initial (make-text-line 'Blank 'Text)) true)
      (check-exception (source-table-row-forms table 'content '(x x)) true)
      (check-exception (source-table-row-forms table #f '(x)) true)
      (check-exception (source-table-row-forms table escape-byte: 256) true)
      (check-exception (source-table-row-forms table rule-marker: 61) true))))
