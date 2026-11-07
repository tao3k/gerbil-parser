;;; One independent workload: checked construction of fresh GrammarRole/Grammar pairs.
(import (only-in :gerbil-parser/src/declarations
                 make-grammar-role make-grammar grammar? grammar-roles grammar-role?)
        (only-in "../../fixtures/progress.ss" report-test-progress!)
        (only-in :asp-gerbil-scheme/src/benchmark/framework
                 benchmark-contract-valid? benchmark-contract-run
                 benchmark-contract-receipt-pass?))
(export make-observed-grammar-batch observed-grammar-batch-valid?
        run-grammar-construction-benchmark main)

(def benchmark-path "t/benchmarks/poo-grammar-objects/benchmark.ss")

(def (make-observed-role)
  (make-grammar-role
   'observability-role '() '() '() '() '() '() '() '() '()))

(def (make-observed-grammar)
  (let (role (make-observed-role))
    (make-grammar 'observability-grammar '() (list role))))

(def (make-observed-grammar-batch)
  (let loop ((remaining 50) (grammars '()))
    (if (zero? remaining)
      (begin (report-test-progress! "OBJECT-BATCH-OK grammars=50") grammars)
      (loop (- remaining 1)
            (cons (make-observed-grammar) grammars)))))

(def (observed-grammar-batch-valid? grammars)
  (and (= (length grammars) 50)
       (andmap
        (lambda (grammar)
          (and (grammar? grammar)
               (= (length (grammar-roles grammar)) 1)
               (grammar-role? (car (grammar-roles grammar)))))
        grammars)))

(def (run-grammar-construction-benchmark)
  (unless (benchmark-contract-valid? benchmark-path)
    (error "invalid grammar construction benchmark contract" benchmark-path))
  (unless (observed-grammar-batch-valid? (make-observed-grammar-batch))
    (error "grammar construction warmup changed the workload result"))
  (##gc)
  (benchmark-contract-run benchmark-path make-observed-grammar-batch))

(def (main)
  (let (receipt (run-grammar-construction-benchmark))
    (write receipt) (newline) (force-output)
    (unless (benchmark-contract-receipt-pass? receipt)
      (error "grammar construction benchmark exceeded its declared budget"))
    (displayln "GRAMMAR-CONSTRUCTION-BENCHMARK-OK") (force-output)))
