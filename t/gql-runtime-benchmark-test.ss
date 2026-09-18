#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Runtime regression gate for the full ISO GQL lexical catalog.

(import :std/test
        (only-in :std/source this-source-file)
        :asp-gerbil-scheme/src/benchmark/framework
        (only-in :gerbil-parser/languages/gql/iso-39075-2024/grammar
                 +gql-representative-query+)
        (only-in :gerbil-parser/languages/gql/iso-39075-2024/parser
                 parse-gql-iso-39075-2024)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success?))

(def benchmark-path
  (path-expand "benchmarks/gql-runtime/benchmark.ss"
               (path-directory (this-source-file))))
(def +parse-count+ 100)

(def (parse-gql-batch)
  (let loop ((remaining +parse-count+))
    (unless (zero? remaining)
      (unless
       (parse-artifact-success?
        (parse-gql-iso-39075-2024 +gql-representative-query+))
       (error "GQL runtime benchmark parse failed"))
      (loop (- remaining 1)))))

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
        (check (benchmark-contract-receipt-pass? receipt) => #t)))))

(run-tests! gql-runtime-benchmark-tests)
