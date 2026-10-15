(import :std/test
        (only-in :gerbil-parser/src/runtime/lr-parser/prepare prepare-lr-goto-columns))
(export lr-goto-plan-test)
(def lr-goto-plan-test
  (test-suite "bounded prepared LR goto columns"
    (test-case "first duplicates, missing cells and shared LHS columns are exact"
      (let* ((table '#((production root) (production root) (production other)))
             (rows '#(((root . 1) (root . 9) (other . 2)) ((other . 3))))
             (columns (prepare-lr-goto-columns table rows)))
        (check columns => '#(#(1 #f) #(1 #f) #(2 3)))
        (check (eq? (vector-ref columns 0) (vector-ref columns 1)) => #t)
        (check (prepare-lr-goto-columns table rows 3) => #f)
        (check (prepare-lr-goto-columns table rows 4) => columns)))
    (test-case "cell ceiling admits its boundary and refuses wider grammars"
      (let* ((table (list->vector (map (lambda (i) (list 'production (string->symbol (number->string i)))) (iota 256))))
             (columns (prepare-lr-goto-columns table (make-vector 256 '()))))
        (check (vector-length columns) => 256)
        (check (andmap (lambda (column) (= (vector-length column) 256)) (vector->list columns)) => #t)
        (check (prepare-lr-goto-columns table (make-vector 257 '())) => #f)))))
