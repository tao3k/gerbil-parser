(import :gerbil-parser/language-test-support)
(import :std/test ../src/runtime/fold-cost)

(deflanguage-parser-tests fold-cost-test "Request-scoped Fold diagnostics"
  (engine)
    (scenario "disabled result and exception semantics"
      (act (fold-cost-count! 'disabled))
      (equal (call-with-fold-cost 'disabled (lambda () 42))  42)
      (raises (call-with-fold-cost 'disabled (lambda () (error "expected"))) true))
    (scenario "nested counts timing and exception restoration"
      (scope ((session (make-fold-cost-session)))
        (parameters ((current-fold-cost-session session))
          (act (fold-cost-count! 'count-only))
          (act (fold-cost-count! 'count-only))
          (act (fold-cost-count! 'count-only 3))
          (raises (fold-cost-count! 'count-only -1) true)
          (equal (call-with-fold-cost 'parent
                   (lambda () (call-with-fold-cost 'child (lambda () 7))))  7)
          (raises (call-with-fold-cost 'failed (lambda () (error "expected"))) true)
          (equal (call-with-fold-cost 'after (lambda () 9))  9))
        (scope ((rows (fold-cost-rows session)))
          (equal (length rows)  5)
          (equal (cdr (assq 'count-only rows))  '(5 0 0 0))
          (equal (list-ref (assq 'failed rows) 4)  1)
          (each (row) (rows)
                      (equal (cadr row)  (if (eq? (car row) 'count-only) 5 1))
                      (equal (>= (caddr row) (cadddr row) 0)  #t))))))
