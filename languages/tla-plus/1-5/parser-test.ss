#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :gerbil-parser/languages/tla-plus/1-5/parser
        :gerbil-parser/src/runtime/artifact
        :gerbil-parser/src/runtime/cst
        ../../support/fixture
        ./fixtures)
(export tla-plus-1-5-parser-test)

(def (cst-node-kinds value)
  (cond
   ((syntax-node? value)
    (cons (syntax-node-kind value)
          (apply append (map cst-node-kinds (syntax-node-children value)))))
   ((syntax-field? value)
    (apply append (map cst-node-kinds (syntax-field-children value))))
   (else '())))

(def (artifact-token-count artifact kind)
  (length
   (filter (lambda (event)
             (and (token-event? event)
                  (eq? (token-event-token-kind event) kind)))
           (parse-artifact-events artifact))))

(def tla-plus-1-5-parser-test
  (test-suite "TLA+ 1.5 versioned language pack"
    (test-case "grammar and corpus identities are immutable"
      (check +tla-plus-language-version+ => "1.5.0")
      (check +tla-plus-syntax-contract-v1+ => "tla-plus.module-core.v1")
      (check +tla-plus-grammar-oracle-version+ => "tree-sitter-tlaplus-1.5.0")
      (check +tla-plus-grammar-oracle-commit+
             => "8a8413f1d08e7ee40b347206d26eac4324db9fd9")
      (check +tla-plus-examples-commit+
             => "ceeaa904140e3e03781cb2a79cd6c6d8b8b08e10")
      (check (syntax-fixture-source-digest (car tla-plus-1-5-fixtures))
             => "sha256:985903176db4725f9cf25df84ad84dcd53ba94be80d26298b87ec86fbc9b08b3"))
    (test-case "all admitted modules publish lossless structural CSTs"
      (for-each
       (lambda (fixture)
         (let* ((source (syntax-fixture-source fixture))
                (artifact (parse-tla-plus-1-5 source))
                (root (and (parse-artifact-success? artifact)
                           (parse-artifact->cst artifact)))
                (kinds (and root (cst-node-kinds root))))
           (check (parse-artifact-success? artifact) => #t)
           (check (parse-artifact-valid? artifact) => #t)
           (check (parse-artifact-roundtrip artifact) => source)
           (check (syntax-node-kind root) => 'SourceFile)
           (for-each
            (lambda (kind) (check (member kind kinds) ? values))
            (syntax-fixture-required-kinds fixture))))
       tla-plus-1-5-fixtures))
    (test-case "nested block comments remain one lossless trivia token"
      (let* ((fixture (caddr tla-plus-1-5-fixtures))
             (artifact (parse-tla-plus-1-5
                        (syntax-fixture-source fixture))))
        (check (parse-artifact-success? artifact) => #t)
        (check (artifact-token-count artifact 'comment) => 1)))
    (test-case "unterminated nested comments fail as one typed artifact"
      (let (artifact
            (parse-tla-plus-1-5
             "---- MODULE Broken ----\n(* outer (* nested *)\nVARIABLE x\n====\n"))
        (check (parse-artifact-success? artifact) => #f)
        (check (parse-artifact-valid? artifact) => #t)
        (check (length (parse-artifact-ref artifact 'diagnostics)) => 1)))))

(run-tests! tla-plus-1-5-parser-test)
