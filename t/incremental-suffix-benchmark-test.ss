#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :asp-gerbil-scheme/src/benchmark/framework
        "./scenarios/performance/incremental-suffix/scenario")

(def benchmark-path
  "t/scenarios/performance/incremental-suffix/benchmark.ss")

(def (run-scenario)
  (unless (incremental-suffix-scenario-pass?
           (incremental-suffix-scenario))
    (error "incremental suffix convergence scenario failed")))

(def incremental-suffix-benchmark-tests
  (test-suite "incremental suffix convergence complexity scenario"
    (test-case "one hundred operands resume and reuse both sides of the edit"
      (check (benchmark-contract-valid? benchmark-path) => #t)
      (let (algorithm-receipt (incremental-suffix-scenario))
        (write algorithm-receipt)
        (newline)
        (force-output)
        (check (incremental-suffix-scenario-pass? algorithm-receipt) => #t))
      (run-scenario)
      (##gc)
      (let (benchmark-receipt
            (benchmark-contract-run benchmark-path run-scenario))
        (write benchmark-receipt)
        (newline)
        (force-output)
        (check (benchmark-contract-receipt-pass? benchmark-receipt) => #t)))))

(def incremental-suffix-benchmark-test incremental-suffix-benchmark-tests)
(export incremental-suffix-benchmark-tests incremental-suffix-benchmark-test)
