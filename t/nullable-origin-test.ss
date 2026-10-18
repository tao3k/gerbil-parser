(import :gerbil-parser/language-test-support
        (only-in :gerbil-parser/t/fixtures/event-walk-source source-procedure)
        (only-in :gerbil-parser/src/compiler/lr
          lower-rules production-table compute-nullable
          validate-resolved-repetitions current-grammar-source-map))

(defparser-check (duplicate-origin-case alternatives expected)
  (sequential ((rules (append '((start (repeat (reference item))))
                      (map (lambda (expression) (list 'item expression)) alternatives)
                      '((exit (empty)))))
               (nullable (call-with-values
                 (lambda () (compute-nullable (lower-rules rules 'start)))
                 (lambda (_names index) index))))
    (scope ((result (parameterize ((current-grammar-source-map '((rule))))
      (with-catch (lambda (condition) (list (error-message condition) (error-irritants condition)))
        (lambda () (validate-resolved-repetitions rules nullable) 'accepted)))))
      (equal (car result) "resolved repetition operand accepts empty input")
      (scope ((details (list-ref (cadr result) 3)))
        (equal (map (lambda (row) (cdr (assq 'rule row)))
                    (cdr (assq 'nullableReferencePath details))) expected)))))

(defparser-check (complete-index-case count reversed?)
  (sequential ((indexer (source-procedure "src/compiler/lr.ss" 'rule-index-by-lhs '()))
               (rows (map (lambda (index)
                       (list (if (even? index) 'a 'b)
                             (if (zero? (modulo index 3)) '(empty) (list 'literal (number->string index)))))
                     (iota count)))
               (ordered (if reversed? (reverse rows) rows))
               (index (indexer ordered)))
    (each (name) ('(a b absent))
      (equal (table-ref index name '())
        (map cadr (filter (lambda (row) (eq? (car row) name)) ordered))))))

(deflanguage-parser-tests nullable-origin-test "complete nullable declaration witnesses"
  (engine)
  (matrix "source table evaluation preserves exact bucket order and multiplicity"
    (setup) ((count '(0 1 2 8 32)) (reversed? '(#f #t))) (bindings)
    (equal (parser-check-result complete-index-case count reversed?) (void)))
  (matrix "a consuming duplicate never hides a nullable declaration"
    (setup) ((alternatives '(((empty) (literal "x"))
                             ((literal "x") (empty))))) (bindings)
    (equal (parser-check-result duplicate-origin-case alternatives '(item)) (void)))
  (matrix "nullable reference exits survive duplicates and cyclic siblings"
    (setup) ((alternatives '(((reference exit) (literal "x"))
                             ((literal "x") (reference exit))
                             ((reference item) (reference exit))
                             ((reference exit) (reference item))))) (bindings)
    (equal (parser-check-result duplicate-origin-case alternatives '(item exit)) (void))))
