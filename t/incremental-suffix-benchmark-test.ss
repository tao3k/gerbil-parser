#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
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

(deflanguage-parser-tests incremental-suffix-benchmark-tests "incremental suffix convergence complexity scenario"
  (engine (exclusive #t))
    (scenario "one hundred operands resume and reuse both sides of the edit"
      (equal (benchmark-contract-valid? benchmark-path)  #t)
      (scope ((algorithm-receipt (incremental-suffix-scenario)))
        (receipt algorithm-receipt)
        (equal (incremental-suffix-scenario-pass? algorithm-receipt)  #t))
      (act (run-scenario))
      (act (##gc))
      (scope ((benchmark-receipt
            (benchmark-contract-run benchmark-path run-scenario)))
        (receipt benchmark-receipt)
        (equal (benchmark-contract-receipt-pass? benchmark-receipt)  #t))))

(def incremental-suffix-benchmark-test incremental-suffix-benchmark-tests)
(export  incremental-suffix-benchmark-test)

;; Resource admission owns its measurement interval in the shared runtime.
