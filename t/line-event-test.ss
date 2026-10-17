(import :gerbil-parser/language-test-support)
;;; Native execution of non-Org declarations through the public engine surface.
(import :std/test
        (only-in :gerbil-parser/src/modules/parser/interface
                 make-table-line make-source-event-scope make-text-line source-table-row-initial
                 source-table-row-forms source-paragraph-initial
                 source-paragraph-close-form source-paragraph-line-form)
        (only-in :gerbil-parser/src/compiler/event-fold-runtime run-event-fold))
(def scope (make-source-event-scope 'native-test))

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
  (run-event-fold source 'Document (source-table-row-initial table scope)
                 (source-table-row-forms table scope escape-byte: escape
                                         rule-bytes: '(61 9 32) rule-marker: 61)
                 '()))
(def (paragraph-events source)
  (run-event-fold source 'Document (source-paragraph-initial text scope)
                 (list (source-paragraph-line-form text scope))
                 (list (source-paragraph-close-form text scope #f '() #f))))
(deflanguage-parser-tests line-event-test "native declarative line events"
  (engine)
    (scenario "non-Org delimiter, escape, empty cell and Unicode source"
      (sequential ((source ";λ!;b;;z;\r\n") (events (table-events source)))
        (equal (starts events 'Cell)  3)
        (equal (source-covered? events source)  #t)))
    (scenario "escape parity and no-escape policy are declaration driven"
      (equal (starts (table-events ";a!!;b;") 'Cell)  2)
      (equal (starts (table-events ";a!;b;" #f) 'Cell)  2)
      (equal (starts (table-events ";a!;b;") 'Cell)  1))
    (scenario "rule-row marker is language policy, not hardcoded Org bytes"
      (equal (starts (table-events ";===;\n") 'RuleRow)  1)
      (equal (starts (table-events ";---;\n") 'Row)  1)
      (equal (source-covered? (table-events ";===;\n") ";===;\n")  #t))
    (scenario "consecutive rows reset escape and separator state"
      (sequential ((source ";a!\n;b;\n") (events (table-events source)))
        (equal (starts events 'Row)  2)
        (equal (starts events 'Cell)  2)
        (equal (source-covered? events source)  #t)))
    (property "paragraph lifetime handles CRLF, blank lines and EOF"
      (bindings (source "α\r\nβ\n\nγ") (events (paragraph-events source)))
      (equal (starts events 'Paragraph)  2)
      (equal (starts events 'Blank)  1)
      (equal (source-covered? events source)  #t))
    (scenario "empty source and leading/trailing blank lines stay lossless"
      (each (source) ('("" "\n\n" "\nα\n\n" "α"))
         (equal (source-covered? (paragraph-events source) source)  #t)))
    (scenario "content helpers receive explicit named state parameters"
      (sequential ((forms (source-table-row-forms table scope 'content '(policy)))
             (events (run-event-fold ";x;" 'Document
                        (append (source-table-row-initial table scope) '((policy 7)))
                        forms '()
                        '((content ((policy 0))
                           ((if (uint-equal? (state policy) (uint 7))
                                ((token Value start end))
                                ((token Wrong start end)))) (policy))))))
        (equal (and (member '(token Value 1 2) events) #t)  #t)))
    (scenario "invalid declarations and bindings are rejected before execution"
      (raises (source-table-row-forms #f scope) true)
      (raises (source-paragraph-initial (make-text-line 'Blank 'Text) scope) true)
      (raises (source-table-row-forms table scope 'content '(x x)) true)
      (raises (source-table-row-forms table scope #f '(x)) true)
      (raises (source-table-row-forms table scope escape-byte: 256) true)
      (raises (source-table-row-forms table scope rule-marker: 61) true)))
