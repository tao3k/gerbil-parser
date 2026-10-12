(import :std/test ../src/compiler/event-fold-zero-state)
(export fold-zero-state-test)
(def guard '(uint-positive? (state mode)))
(def branch `(if ,guard ((set-uint mode (uint 2))) ()))
(def fold-zero-state-test
  (test-suite "Guarded zero-state plan"
    (test-case "idle guards simplify only behind external zero guard"
      (let (plan (fold-zero-state-plan (list branch)))
        (check (vector-ref plan 0) => '(mode))
        (check (vector-ref plan 1) => '()))
      (let* ((configuration '(if (uint-positive? (state policy)) () ()))
             (plan (fold-zero-state-plan (list configuration branch))))
        (check (vector-ref plan 0) => '(mode))
        (check (vector-ref plan 1) => (list configuration))))
    (test-case "writes and opaque effects invalidate facts"
      (let (plan (fold-ordinary-state-plan
                   '((if (line-byte-equal? (line-index cursor) 99) ((token Text start end)) ())) 'cursor))
        (check (vector-ref plan 0) => '())
        (check (vector-ref plan 1) => '(99))
        (check (vector-ref plan 2) => '()))
      (let (plan (fold-zero-state-plan `((token Text start end) ,branch)))
        (check (vector-ref plan 1) => '((token Text start end))))
      (check (fold-zero-state-plan `((set-uint mode (uint 1)) ,branch)) => #f)
      (check (fold-zero-state-plan `((opaque-call) ,branch)) => #f))
    (test-case "unknown branch joins preserve only common facts"
      (let* ((forms `((if (bool-state enabled) ((set-uint mode (uint 1))) ()) ,branch))
             (plan (fold-zero-state-plan forms)))
        (check plan => #f)))
    (test-case "unknown predicate effects retain ordered guards"
      (check (fold-zero-state-plan `((if (and (opaque-predicate) ,guard) () ()))) => #f)
      (check (fold-zero-state-plan `((if (opaque-predicate) (,branch) ()))) => #f))
    (test-case "local join binding shadows external facts"
      (check (fold-zero-state-plan `((join-once mode (,branch) ()))) => #f))))
