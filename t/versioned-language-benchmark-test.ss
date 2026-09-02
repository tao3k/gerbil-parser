#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :asp-gerbil-scheme/src/benchmark/framework
        :gerbil-parser/src/reference/gql-iso-39075-2024
        :gerbil-parser/src/reference/hcl-v2-24
        :gerbil-parser/src/runtime/artifact
        :gerbil-parser/src/runtime/parser
        :gerbil-parser/src/testing/fixture
        :gerbil-parser/src/testing/hcl-v2-24-fixtures)

(def benchmark-path "t/benchmarks/versioned-languages/benchmark.ss")

(def (parse-language-batch)
  (def hcl-source
    (syntax-fixture-source hcl-v2-24-representative-fixture))
  (let loop ((remaining 25))
    (unless (zero? remaining)
      (let ((hcl (parse-source hcl-v2-24-parser hcl-source))
            (gql (parse-source gql-iso-parser +gql-representative-query+)))
        (unless (and (parse-artifact-success? hcl)
                     (parse-artifact-success? gql)
                     (equal? (parse-artifact-roundtrip hcl)
                             hcl-source)
                     (equal? (parse-artifact-roundtrip gql)
                             +gql-representative-query+))
          (error "versioned language benchmark parse failed")))
      (loop (- remaining 1)))))

(def versioned-language-benchmark-tests
  (test-suite "versioned language benchmark"
    (test-case "HCL and GQL share the generic parser performance contract"
      (check (benchmark-contract-valid? benchmark-path) => #t)
      (parse-language-batch)
      (##gc)
      (let (receipt
            (benchmark-contract-run benchmark-path parse-language-batch))
        (write receipt)
        (newline)
        (check (benchmark-contract-receipt-pass? receipt) => #t)))))

(run-tests! versioned-language-benchmark-tests)
