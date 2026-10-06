#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import (only-in :gerbil-parser/languages/hcl/parser-test hcl-test-language)
        (only-in :gerbil-parser/languages/cypher/parser-test opencypher-test-language)
        (only-in :gerbil-parser/languages/tla-plus/parser-test tla-plus-core-test-language)
        (only-in :gerbil-parser/language-support/development
                 language-loader-fixtures language-loader-fixture-count)
        (only-in :gerbil-parser/languages/gql/parser-test gql-test-language)
        :std/test
        :asp-gerbil-scheme/src/benchmark/framework
        :gerbil-parser/languages/hcl/parser
        :gerbil-parser/language-support
        :gerbil-parser/languages/gql/parser
        :gerbil-parser/languages/cypher/parser
        :gerbil-parser/languages/tla-plus/parser
        :gerbil-parser/src/runtime/artifact)

(def benchmark-path "t/benchmarks/versioned-languages/benchmark.ss")

(def benchmark-corpus
  (list (cons parse-hcl (language-loader-fixtures hcl-test-language 'accepted))
        (cons parse-gql (language-loader-fixtures gql-test-language 'accepted))
        (cons parse-opencypher (language-loader-fixtures opencypher-test-language 'accepted))
        (cons parse-tla-plus-core (language-loader-fixtures tla-plus-core-test-language 'accepted))))
(def (parse-language-batch)
  (for-each (lambda (group)
    (for-each (lambda (fixture)
      (let* ((source (syntax-fixture-source fixture)) (artifact ((car group) source)))
        (unless (and (parse-artifact-success? artifact) (equal? (parse-artifact-roundtrip artifact) source))
          (error "official corpus benchmark parse failed" (syntax-fixture-id fixture))))) (cdr group))) benchmark-corpus))

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
