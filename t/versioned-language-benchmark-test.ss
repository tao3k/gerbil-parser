#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :asp-gerbil-scheme/src/benchmark/framework
        :gerbil-parser/languages/hcl/v2-24/fixtures
        :gerbil-parser/languages/hcl/v2-24/parser
        :gerbil-parser/languages/support/fixture
        :gerbil-parser/languages/gql/iso-39075-2024/fixtures
        :gerbil-parser/languages/gql/iso-39075-2024/parser
        :gerbil-parser/src/runtime/artifact)

(def benchmark-path "t/benchmarks/versioned-languages/benchmark.ss")

(def (parse-language-batch)
  (def (parse-corpus parse fixtures)
    (for-each
     (lambda (fixture)
       (let* ((source (syntax-fixture-source fixture))
              (artifact (parse source)))
         (unless (and (parse-artifact-success? artifact)
                      (equal? (parse-artifact-roundtrip artifact) source))
           (error "official corpus benchmark parse failed"
                  (syntax-fixture-id fixture)))))
     fixtures))
  (parse-corpus parse-hcl-v2-24 hcl-v2-24-official-accepted-fixtures)
  (parse-corpus parse-gql-iso-39075-2024 gql-iso-official-fixtures))

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
