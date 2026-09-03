#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :asp-gerbil-scheme/src/benchmark/framework
        :gerbil-parser/t/scenarios/performance/lalr-fixed-point/scenario)

(def benchmark-path
  "t/scenarios/performance/lalr-fixed-point/benchmark.ss")

(def (run-scenario-batch)
  (let loop ((remaining 10))
    (unless (zero? remaining)
      (unless (lalr-fixed-point-scenario-pass?
               (lalr-fixed-point-scenario))
        (error "LALR fixed-point scenario contract failed"))
      (loop (- remaining 1)))))

(def lalr-fixed-point-benchmark-tests
  (test-suite "LALR fixed-point performance scenario"
    (test-case "classic grammars retain exactly-once LR(0) state visits"
      (check (benchmark-contract-valid? benchmark-path) => #t)
      (let (algorithm-receipt (lalr-fixed-point-scenario))
        (write algorithm-receipt)
        (newline)
        (check (lalr-fixed-point-scenario-pass? algorithm-receipt) => #t))
      (run-scenario-batch)
      (##gc)
      (let (benchmark-receipt
            (benchmark-contract-run benchmark-path run-scenario-batch))
        (write benchmark-receipt)
        (newline)
        (check (benchmark-contract-receipt-pass? benchmark-receipt) => #t)))))

(run-tests! lalr-fixed-point-benchmark-tests)
