#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :asp-gerbil-scheme/src/benchmark/framework
        :gerbil-parser/languages/hcl/fixtures
        :gerbil-parser/languages/hcl/parser
        :gerbil-parser/language-support
        :gerbil-parser/languages/gql/fixtures
        :gerbil-parser/languages/gql/parser
        :gerbil-parser/languages/cypher/fixtures
        :gerbil-parser/languages/cypher/parser
        :gerbil-parser/languages/tla-plus/fixtures
        :gerbil-parser/languages/tla-plus/parser
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
  (parse-corpus parse-hcl hcl-official-accepted-fixtures)
  (parse-corpus parse-gql gql-official-fixtures)
  (parse-corpus parse-opencypher
                opencypher-accepted-fixtures)
  (parse-corpus parse-tla-plus-core tla-plus-core-accepted-fixtures))

(def versioned-language-benchmark-tests
  (test-suite "versioned language benchmark"
    (test-case "HCL, GQL, openCypher, and TLA+ share the parser performance contract"
      (check (benchmark-contract-valid? benchmark-path) => #t)
      (parse-language-batch)
      (##gc)
      (let (receipt
            (benchmark-contract-run benchmark-path parse-language-batch))
        (write receipt)
        (newline)
        (force-output)
        (check (benchmark-contract-receipt-pass? receipt) => #t)))))

(def versioned-language-benchmark-test versioned-language-benchmark-tests)
(export versioned-language-benchmark-tests versioned-language-benchmark-test)
