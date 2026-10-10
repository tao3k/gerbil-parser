;;; Prepared backend admission and retained runtime identity.
(import :std/test
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 lr-prepare lr-runtime-for-current-semantic-backend
                 current-lr-event-program-enabled? lr-runtime-event-program?
                 lr-runtime-direct-step install-lr-runtime-event-step!))
(export lr-semantic-preparation-test)

(def (event-runtime runtime)
  (parameterize ((current-lr-event-program-enabled? #t))
    (lr-runtime-for-current-semantic-backend runtime)))

(def lr-semantic-preparation-test
  (test-suite "LR semantic preparation"
    (test-case "preparation captures both strategies independently of request preference"
      (let* ((spec (compile-lr-spec '((source-file (token word))) 'source-file))
             (runtime (parameterize ((current-lr-event-program-enabled? #t))
                        (lr-prepare spec)))
             (selected (event-runtime runtime)))
        (check (lr-runtime-event-program? runtime) => #f)
        (check (lr-runtime-event-program? selected) => #t)
        (check (eq? selected runtime) => #f)
        (check (eq? selected (event-runtime runtime)) => #t)
        (check (eq? selected (event-runtime selected)) => #t)
        (parameterize ((current-lr-event-program-enabled? #f))
          (check (eq? runtime (lr-runtime-for-current-semantic-backend runtime)) => #t)
          (check (eq? selected (lr-runtime-for-current-semantic-backend selected)) => #t))
        (check (eq? selected (event-runtime (lr-prepare spec))) => #f)))
    (test-case "generated step installation changes future selection, not retained executors"
      (let* ((runtime (lr-prepare (compile-lr-spec '((source-file (token word))) 'source-file)))
             (retained (event-runtime runtime))
             (step (lambda args (error "test step must not execute"))))
        (install-lr-runtime-event-step! runtime step)
        (let (selected (event-runtime runtime))
          (check (eq? selected retained) => #f)
          (check (eq? selected (event-runtime runtime)) => #t)
          (check (eq? step (lr-runtime-direct-step selected)) => #t)
          (check (lr-runtime-direct-step retained) => #f))
        (check-exception (install-lr-runtime-event-step! runtime step) true)))
    (test-case "dynamic, branching and layout grammars retain recognition fallback"
      (for-each
       (lambda (rules)
         (let (runtime (lr-prepare (compile-lr-spec rules 'source-file 'selective-glr)))
           (for-each (lambda (_) (check (eq? runtime (event-runtime runtime)) => #t))
                     '(first second third))
           (check (lr-runtime-event-program? runtime) => #f)))
       '(((source-file (precedence dynamic 1 (token word))))
         ((source-file (choice (reference first-path) (reference second-path)))
          (first-path (alias First (token word)))
          (second-path (alias Second (token word))))
         ((source-file (sequence (layout-start "|>") (token word) (layout-end "END")))))))))
