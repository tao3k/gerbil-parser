#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-
;;; Run from outside the checkout against an installed GERBIL_PATH. This owner
;;; proves that generated language modules can resolve their packaged IR files.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/languages/cypher/parser
                 parse-opencypher)
        (only-in :gerbil-parser/languages/gql/parser
                 parse-gql)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-roundtrip
                 parse-artifact-success?
                 parse-artifact-valid?))


;; : (-> (-> String Alist) String List)
(def (consumer-receipt parse source)
  (let (artifact (parse source))
    (list (parse-artifact-success? artifact)
          (parse-artifact-valid? artifact)
          (equal? (parse-artifact-roundtrip artifact) source))))

(deflanguage-parser-tests installed-language-consumer-tests "installed GQL and openCypher consumers"
  (engine)
    (property "ISO GQL resolves its installed sidecars"
      (bindings)
      (equal (consumer-receipt
              parse-gql
              "MATCH (n) RETURN n\n")
              '(#t #t #t)))
    (property "openCypher resolves its installed sidecars"
      (bindings)
      (equal (consumer-receipt
              parse-opencypher
              "MATCH (n) RETURN n\n")
              '(#t #t #t))))


;; gxtest discovers only exported names ending in -test.
(def installed-language-consumer-test installed-language-consumer-tests)
(export installed-language-consumer-test)
