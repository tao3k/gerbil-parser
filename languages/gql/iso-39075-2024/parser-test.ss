#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Acceptance owner for immutable OpenGQL identity, complete fixture parsing,
;;; lossless CST projection, and typed malformed-input terminals.

(import (only-in :std/test check test-case test-suite)
        :gerbil-parser/languages/gql/iso-39075-2024/parser
        :gerbil-parser/src/compiler/bound-ir
        :gerbil-parser/src/runtime/artifact
        :gerbil-parser/src/runtime/cst
        (only-in :gerbil-parser/language-support
                 syntax-fixture-expected-status
                 syntax-fixture-id
                 syntax-fixture-required-kinds
                 syntax-fixture-root-kind
                 syntax-fixture-source
                 syntax-fixture-source-digest
                 syntax-fixture-version)
        (only-in ./fixtures gql-iso-official-fixtures))
(export gql-iso-39075-2024-parser-test)

;;; Structural traversal is intentionally generic over CST nodes and fields so
;;; corpus assertions observe emitted kinds without depending on child layout.
;; : (-> CSTValue (List Symbol))
(def (cst-node-kinds value)
  (cond
   ((syntax-node? value)
    (cons (syntax-node-kind value)
          (apply append (map cst-node-kinds (syntax-node-children value)))))
   ((syntax-field? value)
    (apply append (map cst-node-kinds (syntax-field-children value))))
   (else '())))

;; gql-parser-ir-ref
;; : (forall (a) (-> [(Pair Symbol a)] Symbol a))
;; : (-> Alist Symbol Datum)
(def (gql-parser-ir-ref parser-ir key)
  (alet (entry (assq key parser-ir))
    (cdr entry)))

;;; Acceptance couples immutable standard identity, AOT Parser IR, and
;;; lossless runtime artifacts; no fixture is accepted by parse count alone.
;; : TestSuite
(def gql-iso-39075-2024-parser-test
  (test-suite "ISO/IEC 39075:2024 OpenGQL 1.9.0 syntax"
    (test-case "the GQL standard identity is immutable"
      (check +gql-standard-reference+ => "ISO/IEC 39075:2024")
      (check +gql-standard-edition+ => "edition-1-2024-04")
      (check +gql-opengql-reference-version+ => "1.9.0")
      (check +gql-opengql-reference-commit+
             => "16ea71bd320ad07fd2c46a3066afbaef7d226922")
      (check +gql-syntax-contract+
             => "iso-iec-39075-2024.opengql-1.9.0-syntax.v1")
      (check +gql-antlr4-digest+
             => "sha256:e1b4a24c6b88dedddc0a1fff97df0fc30bf118cea51539e26d71c717cb737bbf")
      (check (length (gql-parser-ir-ref gql-iso-parser-ir 'rules)) => 574)
      (check (gql-parser-ir-ref gql-iso-parser-ir 'materialization)
             => 'aot-expansion)
      (check (gql-parser-ir-ref gql-iso-parser-ir 'conflict-policy)
             => 'selective-glr)
      (check (gql-parser-ir-ref gql-iso-parser-ir 'case-insensitive?) => #t)
      (check (bound-grammar-ir-ref gql-iso-bound-grammar-ir 'bindingCount)
             => 1176)
      (let* ((binding
              (bound-grammar-ir-binding
               gql-iso-bound-grammar-ir 'rule 'gqlProgram))
             (source (bound-grammar-ir-ref binding 'source)))
        (check (bound-grammar-ir-ref binding 'bindingId)
               => '((package . gerbil-parser)
                    (grammar . gql-iso-grammar)
                    (namespace . rule)
                    (name . gqlProgram)))
        (check (gql-parser-ir-ref source 'path) => "grammar-source/GQL.g4")
        (check (gql-parser-ir-ref source 'location)
               => "grammar-source/GQL.g4#gqlProgram")
        (check (map (lambda (reference)
                      (gql-parser-ir-ref reference 'name))
                    (bound-grammar-ir-ref binding 'references))
               => '(programActivity sessionCloseCommand sessionCloseCommand)))
      (check (length gql-iso-official-fixtures) => 14))
    (test-case "the generic machine parses multi-clause GQL losslessly"
      (let* ((artifact (parse-gql-iso-39075-2024 +gql-representative-query+))
             (root (parse-artifact->cst artifact))
             (kinds (cst-node-kinds root)))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-valid? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => +gql-representative-query+)
        (check (syntax-node-kind root) => 'GqlProgram)
        (check (= (length (filter (cut eq? <> 'MatchStatement) kinds)) 2)
               => #t)
        (check (member 'NodePattern kinds) ? values)
        (check (member 'EdgePattern kinds) ? values)
        (check (member 'ElementPropertySpecification kinds) ? values)
        (check (member 'PropertyKeyValuePair kinds) ? values)
        (check (member 'ReturnStatement kinds) ? values)))
    (test-case "GQL keywords are case-insensitive without changing source"
      (let* ((source "match (n) return n\n")
             (artifact (parse-gql-iso-39075-2024 source)))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-valid? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => source)))
    (test-case "GQL delimited identifiers and numeric profile parse losslessly"
      (for-each
       (lambda (source)
         (let (artifact (parse-gql-iso-39075-2024 source))
           (check (parse-artifact-success? artifact) => #t)
           (check (parse-artifact-valid? artifact) => #t)
           (check (parse-artifact-roundtrip artifact) => source)))
       '("MATCH (n) RETURN n AS \"a\"\"b\"\n"
         "RETURN 0xFF, 0o77, 0b101, .5, 1., 2.0M\n")))
    (test-case "all openGQL 1.9.0 official examples parse losslessly"
      (for-each
       (lambda (fixture)
         (let* ((source (syntax-fixture-source fixture))
                (artifact (parse-gql-iso-39075-2024 source))
                (accepted? (parse-artifact-success? artifact)))
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
       gql-iso-official-fixtures))
    (test-case "malformed graph patterns reject as one typed artifact"
      (for-each
       (lambda (source)
         (let (artifact (parse-gql-iso-39075-2024 source))
           (check (parse-artifact-success? artifact) => #f)
           (check (parse-artifact-valid? artifact) => #t)
           (check (length (parse-artifact-ref artifact 'diagnostics)) => 1)))
       '("MATCH\n" "CREATE\n" "SESSION\n")))))
