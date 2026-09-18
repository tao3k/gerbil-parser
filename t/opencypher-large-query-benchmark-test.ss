#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :asp-gerbil-scheme/src/benchmark/framework
        "./scenarios/performance/opencypher-large-query/scenario")

(def benchmark-path
  "t/scenarios/performance/opencypher-large-query/benchmark.ss")

(def (run-scenario)
  (unless (opencypher-large-query-scenario-pass?
           (opencypher-large-query-scenario))
    (error "large openCypher query scenario failed")))

(def opencypher-large-query-benchmark-tests
  (test-suite "openCypher large-query complexity scenario"
    (test-case "one hundred clauses remain linear at full scale"
      (check (benchmark-contract-valid? benchmark-path) => #t)
      (let (algorithm-receipt (opencypher-large-query-scenario))
        (write algorithm-receipt)
        (newline)
        (check (opencypher-large-query-scenario-pass? algorithm-receipt)
               => #t))
      (run-scenario)
      (##gc)
      (let (benchmark-receipt
            (benchmark-contract-run benchmark-path run-scenario))
        (write benchmark-receipt)
        (newline)
        (check (benchmark-contract-receipt-pass? benchmark-receipt) => #t)))))

(run-tests! opencypher-large-query-benchmark-tests)
