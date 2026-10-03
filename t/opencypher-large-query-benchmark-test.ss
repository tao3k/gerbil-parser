#!/usr/bin/env gxi
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

(def opencypher-large-query-benchmark-tests
  (test-suite "openCypher large-query complexity scenario"
    (test-case "one hundred clauses remain linear at full scale"
      (check (benchmark-contract-valid? benchmark-path) => #t)
      (let (algorithm-receipt (opencypher-large-query-scenario))
        (write algorithm-receipt)
        (newline)
        (force-output)
        (check (opencypher-large-query-scenario-pass? algorithm-receipt)
               => #t))
      (run-scenario)
      (##gc)
      (let (benchmark-receipt
            (benchmark-contract-run benchmark-path run-scenario))
        (write benchmark-receipt)
        (newline)
        (force-output)
        (check (benchmark-contract-receipt-pass? benchmark-receipt) => #t)))))

(def opencypher-large-query-benchmark-test
  opencypher-large-query-benchmark-tests)
(export opencypher-large-query-benchmark-tests
        opencypher-large-query-benchmark-test)
