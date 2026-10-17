(import :gerbil-parser/language-test-support)
(import :std/test
        (only-in :gerbil-parser/src/runtime/lr-parser/prepare prepare-lr-goto-columns))

(deflanguage-parser-tests lr-goto-plan-test "bounded prepared LR goto columns"
  (engine)
    (property "first duplicates, missing cells and shared LHS columns are exact"
      (bindings (table '#((production root) (production root) (production other))) (rows '#(((root . 1) (root . 9) (other . 2)) ((other . 3)))) (columns (prepare-lr-goto-columns table rows)))
      (equal columns  '#(#(1 #f) #(1 #f) #(2 3)))
      (equal (eq? (vector-ref columns 0) (vector-ref columns 1))  #t)
      (equal (prepare-lr-goto-columns table rows 3)  #f)
      (equal (prepare-lr-goto-columns table rows 4)  columns))
    (property "cell ceiling admits its boundary and refuses wider grammars"
      (bindings (table (list->vector (map (lambda (i) (list 'production (string->symbol (number->string i)))) (iota 256)))) (columns (prepare-lr-goto-columns table (make-vector 256 '()))))
      (equal (vector-length columns)  256)
      (equal (andmap (lambda (column) (= (vector-length column) 256)) (vector->list columns))  #t)
      (equal (prepare-lr-goto-columns table (make-vector 257 '()))  #f)))
