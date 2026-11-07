#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
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

(deflanguage-parser-tests recovery-frontier-benchmark-tests "LR failure-frontier recovery complexity scenario"
  (engine)
    (scenario "one hundred operands reuse the accepted LR prefix"
      (equal (benchmark-contract-valid? benchmark-path)  #t)
      (scope ((algorithm-receipt (recovery-frontier-scenario)))
        (receipt algorithm-receipt)
        (equal (recovery-frontier-scenario-pass? algorithm-receipt)  #t))
      (act (run-scenario))
      (act (##gc))
      (scope ((benchmark-receipt
            (benchmark-contract-run benchmark-path run-scenario)))
        (receipt benchmark-receipt)
        (equal (benchmark-contract-receipt-pass? benchmark-receipt)  #t))))

(def recovery-frontier-benchmark-test recovery-frontier-benchmark-tests)
(export  recovery-frontier-benchmark-test)

;; Resource admission owns its measurement interval in the shared runtime.
