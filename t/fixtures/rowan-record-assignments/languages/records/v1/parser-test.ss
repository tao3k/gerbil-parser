;;; -*- Gerbil -*-
;;; Native conformance for the record assignments language pack.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/language-support
                 +language-parser-entry-schema+
                 language-parser-entry-ref
                 parse-artifact-ref
                 parse-artifact-roundtrip
                 parse-artifact-success?
                 parse-artifact-valid?
                 syntax-fixture-expected-status
                 syntax-fixture-source
                 syntax-fixture-source-digest)
        (only-in ./fixtures records-v1-fixtures)
        (only-in ./parser records-v1-language parse-records-v1))
(export records-v1-parser-tests)

(def records-v1-parser-tests
  (test-suite "record assignments language pack"
    (test-case "the public entry owns immutable language identity"
      (check (language-parser-entry-ref records-v1-language 'schema)
             => +language-parser-entry-schema+)
      (check (language-parser-entry-ref records-v1-language 'language)
             => "record-assignments")
      (check (language-parser-entry-ref records-v1-language 'version) => "v1")
      (check (language-parser-entry-ref records-v1-language 'contract)
             => "record-assignments.v1"))
    (test-case "accepted and rejected fixtures share one declared corpus"
      (for-each
       (lambda (fixture)
         (let* ((source (syntax-fixture-source fixture))
                (artifact (parse-records-v1 source))
                (accepted? (eq? (syntax-fixture-expected-status fixture)
                                 'accepted)))
           (check (parse-artifact-success? artifact) => accepted?)
           (check (parse-artifact-valid? artifact) => #t)
           (check (parse-artifact-ref artifact 'sourceDigest)
                  => (syntax-fixture-source-digest fixture))
           (when accepted?
             (check (parse-artifact-roundtrip artifact) => source))))
       records-v1-fixtures))))

;; gxtest discovers only exported names ending in -test.
(def parser-test records-v1-parser-tests)
(export parser-test)
