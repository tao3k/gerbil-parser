#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-

(import :std/test
        :asp-gerbil-scheme/src/benchmark/framework
        "./scenarios/performance/opencypher-large-query/scenario"
        (only-in :gerbil-parser/t/fixtures/progress report-test-progress!))

(def benchmark-path
  "t/scenarios/performance/opencypher-large-query/benchmark.ss")

(def (run-scenario)
  (unless (opencypher-large-query-scenario-pass?
           (opencypher-large-query-scenario))
    (error "large openCypher query scenario failed"))
  (when (equal? (getenv "GERBIL_PARSER_LR_TRACE" #f) "1")
    (report-test-progress! "SCENARIO-OK opencypher-large-query clauses=100")
    (force-output)))

(deflanguage-parser-tests opencypher-large-query-benchmark-tests "openCypher large-query complexity scenario"
  (engine)
    (scenario "one hundred clauses remain linear at full scale"
      (equal (benchmark-contract-valid? benchmark-path)  #t)
      (scope ((algorithm-receipt (opencypher-large-query-scenario)))
        (receipt algorithm-receipt)
        (equal (opencypher-large-query-scenario-pass? algorithm-receipt)
                #t))
      (act (run-scenario))
      (act (##gc))
      (scope ((benchmark-receipt
            (benchmark-contract-run benchmark-path run-scenario)))
        (receipt benchmark-receipt)
        (equal (benchmark-contract-receipt-pass? benchmark-receipt)  #t))))

(def opencypher-large-query-benchmark-test
  opencypher-large-query-benchmark-tests)
(export
        opencypher-large-query-benchmark-test)

;; Resource admission owns its measurement interval in the shared runtime.
