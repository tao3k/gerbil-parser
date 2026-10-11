#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :asp-gerbil-scheme/src/benchmark/framework
        "./scenarios/performance/selective-glr/scenario"
        (only-in :gerbil-parser/t/fixtures/progress report-test-progress!))

(def benchmark-path
  "t/scenarios/performance/selective-glr/benchmark.ss")

(def (run-scenario-batch)
  (let loop ((remaining 100))
    (unless (zero? remaining)
      (unless (selective-glr-scenario-pass? (selective-glr-scenario))
        (error "selective GLR correctness scenario failed"))
      (loop (- remaining 1))))
  (when (equal? (getenv "GERBIL_PARSER_LR_TRACE" #f) "1")
    (report-test-progress! "SCENARIO-BATCH-OK selective-glr groups=100")
    (force-output)))

(def selective-glr-benchmark-tests
  (test-suite "selective GLR completion scenario"
    (test-case "static and dynamic forks retain complete bounded evidence"
      (check (benchmark-contract-valid? benchmark-path) => #t)
      (let (algorithm-receipt (selective-glr-scenario))
        (write algorithm-receipt)
        (newline)
        (force-output)
        (check (selective-glr-scenario-pass? algorithm-receipt) => #t))
      (run-scenario-batch)
      (##gc)
      (let (benchmark-receipt
            (benchmark-contract-run benchmark-path run-scenario-batch))
        (write benchmark-receipt)
        (newline)
        (force-output)
        (check (benchmark-contract-receipt-pass? benchmark-receipt) => #t)))))

(def selective-glr-benchmark-test selective-glr-benchmark-tests)

(export selective-glr-benchmark-tests selective-glr-benchmark-test)
