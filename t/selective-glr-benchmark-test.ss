#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :asp-gerbil-scheme/src/benchmark/framework
        "./scenarios/performance/selective-glr/scenario")

(def benchmark-path
  "t/scenarios/performance/selective-glr/benchmark.ss")

(def (run-scenario-batch)
  (let loop ((remaining 100))
    (unless (zero? remaining)
      (unless (selective-glr-scenario-pass? (selective-glr-scenario))
        (error "selective GLR correctness scenario failed"))
      (loop (- remaining 1)))))

(def selective-glr-benchmark-tests
  (test-suite "selective GLR completion scenario"
    (test-case "static and dynamic forks retain complete bounded evidence"
      (check (benchmark-contract-valid? benchmark-path) => #t)
      (let (algorithm-receipt (selective-glr-scenario))
        (write algorithm-receipt)
        (newline)
        (check (selective-glr-scenario-pass? algorithm-receipt) => #t))
      (run-scenario-batch)
      (##gc)
      (let (benchmark-receipt
            (benchmark-contract-run benchmark-path run-scenario-batch))
        (write benchmark-receipt)
        (newline)
        (check (benchmark-contract-receipt-pass? benchmark-receipt) => #t)))))

(export selective-glr-benchmark-tests)
