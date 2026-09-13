#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Run from outside the checkout against an installed GERBIL_PATH. This owner
;;; proves that generated language modules can resolve their packaged IR files.

(import (only-in :std/test check run-tests! test-case test-suite)
        (only-in :gerbil-parser/languages/cypher/opencypher-2024-1/parser
                 parse-opencypher-2024-1)
        (only-in :gerbil-parser/languages/gql/iso-39075-2024/parser
                 parse-gql-iso-39075-2024)
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
              parse-gql-iso-39075-2024
              "MATCH (n) RETURN n\n")
             => '(#t #t #t)))
    (test-case "openCypher resolves its installed sidecars"
      (check (consumer-receipt
              parse-opencypher-2024-1
              "MATCH (n) RETURN n\n")
             => '(#t #t #t)))))

(run-tests! installed-language-consumer-tests)
