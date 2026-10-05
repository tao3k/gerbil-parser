#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Runtime regression gate for the full ISO GQL lexical catalog.

(import :std/test
        (only-in :std/source this-source-file)
        :asp-gerbil-scheme/src/benchmark/framework
        (only-in :gerbil-parser/languages/gql/grammar
                 +gql-representative-query+)
        (only-in :gerbil-parser/languages/gql/parser
                 parse-gql)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success?)
        (only-in :gerbil-parser/t/fixtures/progress report-test-progress!))

(def benchmark-path
  (path-expand "benchmarks/runtime/benchmark.ss"
               (path-directory (this-source-file))))
(def +parse-count+ 100)

(def (parse-gql-batch)
  (let loop ((remaining +parse-count+))
    (unless (zero? remaining)
      (unless
       (parse-artifact-success?
        (parse-gql +gql-representative-query+))
       (error "GQL runtime benchmark parse failed"))
      (loop (- remaining 1))))
  (report-test-progress! "GQL-BATCH-OK parses=" +parse-count+))

(def gql-runtime-benchmark-tests
  (test-suite "ISO GQL parser runtime benchmark"
    (test-case "one hundred GQL parses satisfy the runtime contract"
      (check (benchmark-contract-valid? benchmark-path) => #t)
      (parse-gql-batch)
      (##gc)
      (let (receipt
            (benchmark-contract-run benchmark-path parse-gql-batch))
        (write receipt)
        (newline)
        (force-output)
        (check (benchmark-contract-receipt-pass? receipt) => #t)))))

(def gql-runtime-benchmark-test gql-runtime-benchmark-tests)
(export gql-runtime-benchmark-tests gql-runtime-benchmark-test)
