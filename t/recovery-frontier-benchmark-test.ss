#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :asp-gerbil-scheme/src/benchmark/framework
        "./scenarios/performance/recovery-frontier/scenario")

(def benchmark-path
  "t/scenarios/performance/recovery-frontier/benchmark.ss")

(def (run-scenario)
  (unless (recovery-frontier-scenario-pass?
           (recovery-frontier-scenario))
    (error "LR failure-frontier recovery scenario failed")))

(def recovery-frontier-benchmark-tests
  (test-suite "LR failure-frontier recovery complexity scenario"
    (test-case "one hundred operands reuse the accepted LR prefix"
      (check (benchmark-contract-valid? benchmark-path) => #t)
      (let (algorithm-receipt (recovery-frontier-scenario))
        (write algorithm-receipt)
        (newline)
        (force-output)
        (check (recovery-frontier-scenario-pass? algorithm-receipt) => #t))
      (run-scenario)
      (##gc)
      (let (benchmark-receipt
            (benchmark-contract-run benchmark-path run-scenario))
        (write benchmark-receipt)
        (newline)
        (force-output)
        (check (benchmark-contract-receipt-pass? benchmark-receipt) => #t)))))

(def recovery-frontier-benchmark-test recovery-frontier-benchmark-tests)
(export recovery-frontier-benchmark-tests recovery-frontier-benchmark-test)
