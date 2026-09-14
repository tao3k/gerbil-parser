#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Acceptance owner for pinned TLA+ sources, structural CST obligations,
;;; nested-comment losslessness, and typed unterminated-comment failure.

(import (only-in :std/test check test-case test-suite)
        :gerbil-parser/languages/tla-plus/v1/parser
        :gerbil-parser/src/runtime/artifact
        :gerbil-parser/src/runtime/cst
        (only-in :gerbil-parser/language-support
                 syntax-fixture-required-kinds
                 syntax-fixture-source
                 syntax-fixture-source-digest)
        (only-in ./fixtures tla-plus-v1-fixtures))
(export tla-plus-v1-parser-test)

;;; CST traversal intentionally treats fields as transparent containers; the
;;; observable contract is the ordered set of emitted syntax-node kinds.
;; : (-> CSTValue (List Symbol))
(def (cst-node-kinds value)
  (cond
   ((syntax-node? value)
    (cons (syntax-node-kind value)
          (apply append (map cst-node-kinds (syntax-node-children value)))))
   ((syntax-field? value)
    (apply append (map cst-node-kinds (syntax-field-children value))))
   (else '())))

;; : (-> ParseArtifact Symbol Integer)
(def (artifact-token-count artifact kind)
  (length
   (filter (lambda (event)
             (and (token-event? event)
                  (eq? (token-event-token-kind event) kind)))
           (parse-artifact-events artifact))))

;; : TestSuite
(def tla-plus-v1-parser-test
  (test-suite "TLA+ v1 versioned language pack"
    (test-case "native syntax and corpus identities are immutable"
      (check +tla-plus-contract-version+ => "v1")
      (check +tla-plus-syntax-contract+ => "tla-plus.module-core.v1")
      (check +tla-plus-syntax-source+
             => "Specifying Systems, Chapter 15: TLAPlusGrammar")
      (check +tla-plus-sany-release+ => "v1.7.4")
      (check +tla-plus-sany-commit+
             => "5a47802b5c391f59ecdd44117981f4ff8c0656ba")
      (check +tla-plus-sany-grammar-blob+
             => "bf9e7acb5337f4b6c2a4d6a973a1a65c95e72f56")
      (check +tla-plus-examples-commit+
             => "ceeaa904140e3e03781cb2a79cd6c6d8b8b08e10")
      (check (syntax-fixture-source-digest (car tla-plus-v1-fixtures))
             => "sha256:985903176db4725f9cf25df84ad84dcd53ba94be80d26298b87ec86fbc9b08b3"))
    (test-case "all admitted modules publish lossless structural CSTs"
      (for-each
       (lambda (fixture)
         (let* ((source (syntax-fixture-source fixture))
                (artifact (parse-tla-plus-v1 source))
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
       tla-plus-v1-fixtures))
    (test-case "nested block comments remain one lossless trivia token"
      (let* ((fixture (caddr tla-plus-v1-fixtures))
             (artifact (parse-tla-plus-v1
                        (syntax-fixture-source fixture))))
        (check (parse-artifact-success? artifact) => #t)
        (check (artifact-token-count artifact 'comment) => 1)))
    (test-case "unterminated nested comments fail as one typed artifact"
      (let (artifact
            (parse-tla-plus-v1
             "---- MODULE Broken ----\n(* outer (* nested *)\nVARIABLE x\n====\n"))
        (check (parse-artifact-success? artifact) => #f)
        (check (parse-artifact-valid? artifact) => #t)
        (check (length (parse-artifact-ref artifact 'diagnostics)) => 1)))))
