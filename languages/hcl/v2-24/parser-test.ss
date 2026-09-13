#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Acceptance owner for the pinned HCL specsuite, descriptor identity,
;;; lossless accepted artifacts, and typed rejected artifacts.

(import (only-in :std/test check test-case test-suite)
        :gerbil-parser/languages/hcl/v2-24/parser
        :gerbil-parser/src/language/entry
        :gerbil-parser/src/runtime/artifact
        :gerbil-parser/src/runtime/cst
        (only-in :gerbil-parser/language-support
                 syntax-fixture-contract
                 syntax-fixture-expected-status
                 syntax-fixture-id
                 syntax-fixture-required-kinds
                 syntax-fixture-root-kind
                 syntax-fixture-source
                 syntax-fixture-source-digest
                 syntax-fixture-version)
        (only-in ./fixtures
                 hcl-v2-24-official-accepted-fixtures
                 hcl-v2-24-official-fixtures
                 hcl-v2-24-official-rejected-fixtures))
(export hcl-v2-24-parser-test)

;;; Structural traversal stays independent of HCL production nesting so the
;;; official corpus checks semantic kinds while roundtrip checks retain bytes.
;; : (-> CSTValue (List Symbol))
(def (cst-node-kinds value)
  (cond
   ((syntax-node? value)
    (cons (syntax-node-kind value)
          (apply append (map cst-node-kinds (syntax-node-children value)))))
   ((syntax-field? value)
    (apply append (map cst-node-kinds (syntax-field-children value))))
   (else '())))

;; : TestSuite
(def hcl-v2-24-parser-test
  (test-suite "HCL native syntax v2.24.0 official corpus"
    (test-case "the upstream syntax identity is immutable"
      (check +hcl-native-syntax-version+ => "v2.24.0")
      (check +hcl-native-syntax-commit+
             => "6b5068090eef06b1f127f61529db5ba0be7ed343")
      (check +hcl-syntax-contract+
             => "hcl-native-v2.24.0.v1")
      (check (language-parser-entry-ref hcl-v2-24-language 'schema)
             => +language-parser-entry-schema+)
      (check (language-parser-entry-ref hcl-v2-24-language 'language)
             => "hcl")
      (check (language-parser-entry-ref hcl-v2-24-language 'version)
             => +hcl-native-syntax-version+)
      (check (language-parser-entry-ref hcl-v2-24-language 'contract)
             => +hcl-syntax-contract+)
      (check (length hcl-v2-24-official-fixtures) => 15)
      (check (length hcl-v2-24-official-accepted-fixtures) => 12)
      (check (length hcl-v2-24-official-rejected-fixtures) => 3))
    (test-case "complete official HCL specsuite sources parse losslessly"
      (for-each
       (lambda (fixture)
         (let* ((source (syntax-fixture-source fixture))
                (artifact (parse-hcl-v2-24 source))
                (accepted? (parse-artifact-success? artifact)))
           (check (syntax-fixture-version fixture) => +hcl-native-syntax-version+)
           (check (syntax-fixture-contract fixture) => +hcl-syntax-contract+)
           (check (syntax-fixture-expected-status fixture) => 'accepted)
           (check (list (syntax-fixture-id fixture) accepted?)
                  => (list (syntax-fixture-id fixture) #t))
           (check (parse-artifact-valid? artifact) => #t)
           (check (parse-artifact-ref artifact 'sourceDigest)
                  => (syntax-fixture-source-digest fixture))
           (check (parse-artifact-roundtrip artifact) => source)
           (when accepted?
             (let* ((root (parse-artifact->cst artifact))
                    (kinds (cst-node-kinds root)))
               (check (syntax-node-kind root)
                      => (syntax-fixture-root-kind fixture))
               (for-each
                (lambda (required-kind)
                  (check (member required-kind kinds) ? values))
                (syntax-fixture-required-kinds fixture))))))
       hcl-v2-24-official-accepted-fixtures))
    (test-case "official invalid HCL sources fail as one typed artifact"
      (for-each
       (lambda (fixture)
         (let* ((source (syntax-fixture-source fixture))
                (artifact (parse-hcl-v2-24 source)))
           (check (syntax-fixture-version fixture) => +hcl-native-syntax-version+)
           (check (syntax-fixture-contract fixture) => +hcl-syntax-contract+)
           (check (syntax-fixture-expected-status fixture) => 'rejected)
           (check (parse-artifact-success? artifact) => #f)
           (check (parse-artifact-valid? artifact) => #t)
           (check (parse-artifact-ref artifact 'sourceDigest)
                  => (syntax-fixture-source-digest fixture))
           (check (parse-artifact-roundtrip artifact) => source)
           (check (length (parse-artifact-ref artifact 'diagnostics)) => 1)))
       hcl-v2-24-official-rejected-fixtures))))
