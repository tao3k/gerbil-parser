#!/usr/bin/env gxi
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
(export installed-language-consumer-tests)

;; : (-> (-> String Alist) String List)
(def (consumer-receipt parse source)
  (let (artifact (parse source))
    (list (parse-artifact-success? artifact)
          (parse-artifact-valid? artifact)
          (equal? (parse-artifact-roundtrip artifact) source))))

(def installed-language-consumer-tests
  (test-suite "installed GQL and openCypher consumers"
    (test-case "ISO GQL resolves its installed sidecars"
      (check (consumer-receipt
              parse-gql
              "MATCH (n) RETURN n\n")
             => '(#t #t #t)))
    (test-case "openCypher resolves its installed sidecars"
      (check (consumer-receipt
              parse-opencypher
              "MATCH (n) RETURN n\n")
             => '(#t #t #t)))))

(export installed-language-consumer-tests)

;; gxtest discovers only exported names ending in -test.
(def installed-language-consumer-test installed-language-consumer-tests)
(export installed-language-consumer-test)
