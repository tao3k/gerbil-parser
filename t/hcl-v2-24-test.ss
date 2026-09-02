#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :gerbil-parser/src/compiler/machine
        :gerbil-parser/src/reference/hcl-v2-24
        :gerbil-parser/src/runtime/artifact
        :gerbil-parser/src/runtime/cst
        :gerbil-parser/src/runtime/parser
        :gerbil-parser/src/testing/fixture
        :gerbil-parser/src/testing/hcl-v2-24-fixtures)

(def (cst-node-kinds value)
  (cond
   ((syntax-node? value)
    (cons (syntax-node-kind value)
          (apply append (map cst-node-kinds (syntax-node-children value)))))
   ((syntax-field? value)
    (apply append (map cst-node-kinds (syntax-field-children value))))
   (else '())))

(def hcl-v2-24-tests
  (test-suite "HCL native syntax v2.24.0 structural core"
    (test-case "the upstream syntax identity is immutable"
      (check +hcl-native-syntax-version+ => "v2.24.0")
      (check +hcl-native-syntax-commit+
             => "6b5068090eef06b1f127f61529db5ba0be7ed343")
      (check +hcl-syntax-contract-v1+
             => "hcl-native-v2.24.0-structural-core.v1"))
    (test-case "one generic machine parses a real-shaped HCL body losslessly"
      (let* ((fixture hcl-v2-24-representative-fixture)
             (source (syntax-fixture-source fixture))
             (artifact (parse-source hcl-v2-24-parser source))
             (root (parse-artifact->cst artifact))
             (kinds (cst-node-kinds root)))
        (check (syntax-fixture-version fixture) => +hcl-native-syntax-version+)
        (check (syntax-fixture-contract fixture) => +hcl-syntax-contract-v1+)
        (check (syntax-fixture-expected-status fixture) => 'accepted)
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-valid? artifact) => #t)
        (check (parse-artifact-ref artifact 'sourceDigest)
               => (syntax-fixture-source-digest fixture))
        (check (parse-artifact-roundtrip artifact) => source)
        (check (syntax-node-kind root) => (syntax-fixture-root-kind fixture))
        (for-each
         (lambda (required-kind)
           (check (member required-kind kinds) ? values))
         (syntax-fixture-required-kinds fixture))))
    (test-case "invalid HCL terminals fail as one typed artifact"
      (for-each
       (lambda (fixture)
         (let* ((source (syntax-fixture-source fixture))
                (artifact (parse-source hcl-v2-24-parser source)))
           (check (syntax-fixture-version fixture) => +hcl-native-syntax-version+)
           (check (syntax-fixture-contract fixture) => +hcl-syntax-contract-v1+)
           (check (syntax-fixture-expected-status fixture) => 'rejected)
           (check (parse-artifact-success? artifact) => #f)
           (check (parse-artifact-valid? artifact) => #t)
           (check (parse-artifact-ref artifact 'sourceDigest)
                  => (syntax-fixture-source-digest fixture))
           (check (parse-artifact-roundtrip artifact) => source)
           (check (length (parse-artifact-ref artifact 'diagnostics)) => 1)))
       hcl-v2-24-invalid-fixtures))))

(run-tests! hcl-v2-24-tests)
