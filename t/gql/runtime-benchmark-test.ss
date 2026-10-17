#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-
;;; Runtime regression gate for the full ISO GQL lexical catalog.

(import :std/test
        (only-in :std/source this-source-file)
        :asp-gerbil-scheme/src/benchmark/framework
        (only-in :gerbil-parser/languages/gql/parser
                 +gql-representative-query+)
        (only-in :gerbil-parser/languages/gql/parser
                 parse-gql)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success?)
        (only-in :gerbil-parser/t/fixtures/progress report-test-progress!))

(def benchmark-path
  (path-expand "../benchmarks/gql/runtime/benchmark.ss"
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

(deflanguage-parser-tests gql-runtime-benchmark-tests "ISO GQL parser runtime benchmark"
  (engine (exclusive #t))
    (scenario "one hundred GQL parses satisfy the runtime contract"
      (equal (benchmark-contract-valid? benchmark-path)  #t)
      (act (parse-gql-batch))
      (act (##gc))
      (scope ((receipt
            (benchmark-contract-run benchmark-path parse-gql-batch)))
        (receipt receipt)
        (equal (benchmark-contract-receipt-pass? receipt)  #t))))

(def gql-runtime-benchmark-test gql-runtime-benchmark-tests)
(export  gql-runtime-benchmark-test)

;; Resource admission owns its measurement interval in the shared runtime.
