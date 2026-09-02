#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Language-neutral compile-time native-source fixture contract.

(import :std/test
        :gerbil-parser/src/reference/arithmetic-v1
        :gerbil-parser/src/runtime/artifact
        :gerbil-parser/src/runtime/parser
        :gerbil-parser/src/testing/fixture)

(defsyntax-fixture downstream-dsl-fixture
  (identity "downstream/example/native-source"
            "downstream example DSL"
            "v1"
            "downstream-example.v1")
  (source "corpus/arithmetic/basic.expr")
  (expect accepted SourceFile ()))

(def syntax-fixture-tests
  (test-suite "declarative native-source fixtures"
    (test-case "a downstream DSL embeds its native file at macro expansion"
      (let* ((fixture downstream-dsl-fixture)
             (source (syntax-fixture-source fixture))
             (artifact (parse-source arithmetic-parser source)))
        (check (syntax-fixture-id fixture)
               => "downstream/example/native-source")
        (check (syntax-fixture-language fixture)
               => "downstream example DSL")
        (check (syntax-fixture-version fixture) => "v1")
        (check (syntax-fixture-contract fixture) => "downstream-example.v1")
        (check (syntax-fixture-expected-status fixture) => 'accepted)
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-ref artifact 'sourceDigest)
               => (syntax-fixture-source-digest fixture))
        (check (parse-artifact-roundtrip artifact) => source)))))

(run-tests! syntax-fixture-tests)
