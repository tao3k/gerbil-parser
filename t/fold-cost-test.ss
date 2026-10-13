(import :std/test ../src/runtime/fold-cost)
(export fold-cost-test)
(def fold-cost-test
  (test-suite "Request-scoped Fold diagnostics"
    (test-case "disabled result and exception semantics"
      (fold-cost-count! 'disabled)
      (check (call-with-fold-cost 'disabled (lambda () 42)) => 42)
      (check-exception (call-with-fold-cost 'disabled (lambda () (error "expected"))) true))
    (test-case "nested counts timing and exception restoration"
      (let (session (make-fold-cost-session))
        (parameterize ((current-fold-cost-session session))
          (fold-cost-count! 'count-only)
          (fold-cost-count! 'count-only)
          (fold-cost-count! 'count-only 3)
          (check-exception (fold-cost-count! 'count-only -1) true)
          (check (call-with-fold-cost 'parent
                   (lambda () (call-with-fold-cost 'child (lambda () 7)))) => 7)
          (check-exception (call-with-fold-cost 'failed (lambda () (error "expected"))) true)
          (check (call-with-fold-cost 'after (lambda () 9)) => 9))
        (let (rows (fold-cost-rows session))
          (check (length rows) => 5)
          (check (cdr (assq 'count-only rows)) => '(5 0 0 0))
          (check (list-ref (assq 'failed rows) 4) => 1)
          (for-each (lambda (row)
                      (check (cadr row) => (if (eq? (car row) 'count-only) 5 1))
                      (check (>= (caddr row) (cadddr row) 0) => #t)) rows))))))
