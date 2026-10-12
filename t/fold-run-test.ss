(import :std/test ../src/compiler/event-fold-ordinary-run
        (only-in ../src/compiler/event-fold-zero-state fold-zero-state-plan))
(export fold-run-test)

(def special '(if (line-byte-equal? (line-index cursor) 58) ((token Text start end)) ()))
(def overwrite '(set-bool seen-space
                   (line-bytes-any-in? (line-index cursor) (line-step (line-index cursor)) (32))))
(def fold-run-test
  (test-suite "Overwrite-only ordinary runs"
    (test-case "byte-dependent overwrites require no state observation"
      (let (plan (fold-ordinary-run-plan (list special overwrite) 'cursor))
        (check (vector-ref plan 1) => '(58))
        (check (vector-ref plan 3) => (list overwrite))))
    (test-case "events state reads queries and frames reject coalescing"
      (for-each
       (lambda (form)
         (check (fold-ordinary-run-plan (list special form) 'cursor) => #f))
       '((token Text start end)
         (set-uint count (uint-add (state count) (uint 1)))
         (set-bool flag (future-heading-title? "x"))
         (with-source-bounds start end ()))))
    (test-case "prefix bound must remain stable throughout the run"
      (let (plan (fold-ordinary-run-plan
                  `((if (offset-less? (line-index cursor) (state-offset bound))
                        () (,special ,overwrite))) 'cursor))
        (check (vector-ref plan 2)
               => '(not (offset-less? (line-index cursor) (state-offset bound)))))
      (check (fold-ordinary-run-plan
               `((if (offset-less? (line-index cursor) (state-offset bound))
                     () (,special (set-uint bound (offset (line-index cursor)))))) 'cursor)
             => #f))
    (test-case "zero cofactoring covers literals but not subsequent writes"
      (let* ((branch '(if (uint-positive? (state mode)) ((set-uint mode (uint 2))) ()))
             (literal '(if (uint-equal? (state mode) (uint 4)) ((token Text start end)) ()))
             (plan (fold-zero-state-plan (list branch literal) #t)))
        (check (vector-ref plan 1) => '())
        (check (fold-zero-state-plan
                 (list branch '(set-uint mode (uint 4)) literal) #t)
               => (vector '(mode) (list '(set-uint mode (uint 4)) literal)))))
    (test-case "region facts come from the explicit entry guard"
      (let (literal '(if (uint-equal? (state mode) (uint 4)) ((token Text start end)) ()))
        (check (vector-ref (fold-zero-state-plan (list literal) #t '(mode)) 1) => '())
        (check (fold-zero-state-plan (list literal) #t '()) => #f)
        (check (fold-zero-state-plan
                 (list '(opaque-call) literal) #t '(mode)) => #f)))))
