#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-

(import :std/test
        :asp-gerbil-scheme/src/benchmark/framework
        "./scenarios/performance/lalr-fixed-point/scenario")

(def benchmark-path
  "t/scenarios/performance/lalr-fixed-point/benchmark.ss")

(def (run-scenario-batch)
  (let loop ((remaining 10))
    (unless (zero? remaining)
      (unless (lalr-fixed-point-scenario-pass?
               (lalr-fixed-point-scenario))
        (error "LALR fixed-point scenario contract failed"))
      (loop (- remaining 1)))))

(deflanguage-parser-tests lalr-fixed-point-benchmark-tests "LALR fixed-point performance scenario"
  (engine)
    (scenario "classic grammars retain exactly-once LR(0) state visits"
      (equal (benchmark-contract-valid? benchmark-path)  #t)
      (scope ((algorithm-receipt (lalr-fixed-point-scenario)))
        (receipt algorithm-receipt)
        (equal (lalr-fixed-point-scenario-pass? algorithm-receipt)  #t))
      (act (run-scenario-batch))
      (act (##gc))
      (scope ((benchmark-receipt
            (benchmark-contract-run benchmark-path run-scenario-batch)))
        (receipt benchmark-receipt)
        (equal (benchmark-contract-receipt-pass? benchmark-receipt)  #t))))

(def lalr-fixed-point-benchmark-test lalr-fixed-point-benchmark-tests)
(export  lalr-fixed-point-benchmark-test)

;; Resource admission owns its measurement interval in the shared runtime.
