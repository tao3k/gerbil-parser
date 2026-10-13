(import :std/test ../src/compiler/event-fold-ordinary-run
        (only-in ../src/compiler/event-fold-scheme-runs fold-scheme-run-loop)
        (only-in ../src/compiler/event-fold-analysis fold-zero-support-preserved?)
        (only-in ../src/compiler/event-fold-zero-state fold-zero-state-plan))
(export fold-run-test)

(def special '(if (line-byte-equal? (line-index cursor) 58) ((token Text start end)) ()))
(def overwrite '(set-bool seen-space
                   (line-bytes-any-in? (line-index cursor) (line-step (line-index cursor)) (32))))
(def counter '(if (line-bytes-any-in? (line-index cursor)
                    (line-step (line-index cursor)) (65 66))
                  ((set-uint count (uint-add (state count) (uint 1))))
                  ((set-uint count (uint 0)))))
(def fold-run-test
  (test-suite "Overwrite-only ordinary runs"
    (test-case "ordered consumer invariant admits only zero preserving writes"
      (check (fold-zero-support-preserved?
               '((if (uint-positive? (state mode)) ((set-uint pending (uint 0))) ()))
               '(pending)) => #t)
      (check (fold-zero-support-preserved? '((call-source-helper unknown)) '(pending)) => #f)
      (check (fold-zero-support-preserved? '((token Text start end)) '(pending)) => #f))
    (test-case "nonzero transient support writes cannot admit a local run"
      (check (fold-zero-support-preserved?
               '((set-uint pending (uint 1)) (set-uint pending (uint 0))) '(pending)) => #f)
      (check (fold-consumer-run-plan
               (list special '(set-uint pending (uint 1))
                     '(if (uint-positive? (state pending)) ((token Text start end)) ())
                     '(set-uint pending (uint 0))) 'cursor '(pending)) => #f))
    (test-case "interval selection retains the narrow inactive alphabet"
      (let (plan (fold-ordinary-run-plan
                   '((if (uint-positive? (state active-mode))
                       ((if (line-byte-equal? (line-index cursor) 65) ((token Text start end)) ())) ())
                     (if (line-byte-equal? (line-index cursor) 58) ((set-uint active-mode (uint 1))) ()))
                   'cursor))
        (check (vector-ref plan 0) => '())
        (check (vector-ref (vector-ref plan 5) 0) => '(active-mode))
        (check (vector-ref (vector-ref plan 5) 1) => '(58))
        (check (and (memv 65 (vector-ref plan 1)) #t) => #t)))
    (test-case "only a dependent consumer keeps its interval premise"
      (let* ((body '((if (uint-positive? (state blocked-mode)) ((token Text start end)) ())
                    (if (uint-positive? (state active-mode)) ((set-uint active-mark (uint 7))) ())
                    (if (line-byte-equal? (line-index cursor) 58)
                         ((set-uint blocked-mode (uint 1)) (set-uint active-mode (uint 1))) ())))
             (plan (fold-ordinary-run-plan body 'cursor)))
        (check (vector-ref plan 0) => '(blocked-mode))
        (check (vector-ref plan 3)
               => '((if (uint-positive? (state active-mode)) ((set-uint active-mark (uint 7))) ())))))
    (test-case "independent active consumers do not need an all-idle guard"
      (let* ((body (list special
                    `(if (uint-positive? (state first-mode)) (,overwrite) ())
                    '(if (uint-positive? (state second-mode))
                       ((set-uint second-mark (uint 7))) ())))
             (plan (fold-consumer-run-plan body 'cursor)))
        (check (vector-ref plan 0) => '())
        (check (vector-ref plan 3) => (cdr body))))
    (test-case "consumer dependencies and changing selectors reject interval execution"
      (for-each (lambda (body) (check (fold-consumer-run-plan body 'cursor) => #f))
        (list (list special '(set-uint mode (uint 1))
                    '(if (uint-positive? (state mode)) ((set-bool flag (bool #t))) ()))
              (list special '(if (line-byte-equal? (line-index cursor) 32)
                                ((set-bool flag (bool #t)))
                                ((set-bool flag (bool #f))))
                    '(if (state flag) ((set-uint mark (uint 1))) ()))
              (list special '(if (future-heading-title? "x")
                                ((set-bool flag (bool #t))) ())))))
    (test-case "counter class no longer forces exceptional state execution"
      (let (plan (fold-ordinary-run-plan (list special counter overwrite) 'cursor))
        (check (vector-ref plan 1) => '(58))
        (check (vector-ref plan 4) => '((count 65 66)))
        (check (vector-ref plan 3) => (list overwrite)))
      ;; The span summary must retain reset/carry behavior, including counters
      ;; beyond the C integer domain; no counter value crosses the leaf ABI.
      (let* ((plan (vector '() '(58) '(bool #t) '()
                          '((count 65 66) (second-count 66 67))))
             (code (fold-scheme-run-loop '(for-each-byte cursor start end ())
                      plan 'start 'end 'cursor 'limit 'byte
                      '((count . count) (second-count . second-count)) '()
                      (lambda (_forms _bindings _indices) '((void))))))
        ;; Independent scalar reference: include NUL (truthy), empty and
        ;; exact-end spans so a speculative out-of-bounds load must fail.
        (for-each
          (lambda (input)
            (let (reference
                   (foldl (lambda (byte counts)
                            (list (if (memv byte '(65 66)) (+ 1 (car counts)) 0)
                                  (if (memv byte '(66 67)) (+ 1 (cadr counts)) 0)))
                          '(18446744073709551616 7) (u8vector->list input)))
              (check (eval `(let ((source-bytes ',input) (start 0)
                                  (end ,(u8vector-length input))
                                  (count 18446744073709551616) (second-count 7))
                             ,code (list count second-count))) => reference)))
          (list #u8() #u8(0) #u8(65 66) #u8(65 0 66) #u8(0 65 66 66)))))
    (test-case "observed counters reject summaries; conditional updates retain their selector"
      (for-each (lambda (body) (check (fold-ordinary-run-plan body 'cursor) => #f))
        (list (list special counter '(set-bool seen (uint-positive? (state count))))
              (list special counter '(set-uint count (uint 9)))
              (list special counter counter)))
      (let (plan (fold-consumer-run-plan (list special `(if (state enabled) (,counter) ())) 'cursor))
        (check (vector-ref plan 4) => '())
        (check (vector-ref plan 3)
               => '((if (state enabled) ((set-uint count (uint 0))) ())))))
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
