#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Acceptance owner for openCypher Grammar IR, Bound IR, and ParseArtifacts.

(import (only-in :std/test check test-case test-suite)
        :gerbil-parser/languages/cypher/opencypher-2024-1/parser
        :gerbil-parser/src/compiler/bound-ir
        (only-in :gerbil-parser/src/compiler/lr
                 lr-spec-ref production-lhs production-rhs)
        (only-in :gerbil-parser/src/compiler/machine parser-machine-ir)
        :gerbil-parser/src/compiler/parser-ir
        :gerbil-parser/src/runtime/artifact
        :gerbil-parser/src/runtime/cst
        (only-in :gerbil-parser/src/runtime/lexer lex-source)
        (only-in :gerbil-parser/src/runtime/lr-parser lr-parse/receipt)
        (only-in :gerbil-parser/src/runtime/significant
                 parser-significant-tokens)
        (only-in :gerbil-parser/language-support
                 syntax-fixture-expected-status
                 syntax-fixture-id
                 syntax-fixture-required-kinds
                 syntax-fixture-root-kind
                 syntax-fixture-source)
        (only-in ./fixtures opencypher-2024-1-fixtures))
(export opencypher-2024-1-parser-test)

;; : (-> CSTValue (List Symbol))
(def (cst-node-kinds value)
  (cond
   ((syntax-node? value)
    (cons (syntax-node-kind value)
          (apply append (map cst-node-kinds (syntax-node-children value)))))
   ((syntax-field? value)
    (apply append (map cst-node-kinds (syntax-field-children value))))
   (else '())))

;; : (forall (a) (-> [(Pair Symbol a)] Symbol a))
;; : (-> Alist Symbol Datum)
(def (bound-row-ref row key)
  (alet (entry (assq key row))
    (cdr entry)))

;; : (-> Fixnum String)
(def (repeated-boolean-list-source count)
  (string-append
   "RETURN [" (string-join (make-list count "true") ", ") "]"))

;; : (-> Datum Boolean)
(def (fork-action-entry? entry)
  (eq? (cadr entry) 'fork))

;;; This suite couples catalog completeness to native Bound IR identities and
;;; lossless runtime artifacts, so a parse total alone cannot satisfy it.
;; : TestSuite
(def opencypher-2024-1-parser-test
  (test-suite "openCypher 2024.1 generated parser"
    (test-case "the complete BNF lowers through Parser IR and Bound IR"
      (check (length (parser-ir-ref opencypher-2024-1-parser-ir 'rules)) => 352)
      (check (parser-ir-ref opencypher-2024-1-parser-ir 'root-rule) => 'program)
      (check (parser-ir-ref opencypher-2024-1-parser-ir 'materialization)
             => 'aot-expansion)
      (check (bound-grammar-ir-ref
              opencypher-2024-1-bound-grammar-ir 'bindingCount)
             => 736)
      (let* ((binding
              (bound-grammar-ir-binding
               opencypher-2024-1-bound-grammar-ir 'rule 'program))
             (source (bound-grammar-ir-ref binding 'source)))
        (check (bound-row-ref source 'path)
               => "grammar-source/openCypher.bnf")
        (check (bound-row-ref source 'line) => 3)
        (check (bound-grammar-ir-ref binding 'references)
               => '(((package . gerbil-parser)
                     (grammar . opencypher-2024-1-grammar)
                     (namespace . rule)
                     (name . |procedure specification|))
                    ((package . gerbil-parser)
                     (grammar . opencypher-2024-1-grammar)
                     (namespace . rule)
                     (name . |standalone procedure call|)))))
      (let* ((binding
              (bound-grammar-ir-binding
               opencypher-2024-1-bound-grammar-ir 'rule
               '|linear statement|))
             (source (bound-grammar-ir-ref binding 'source)))
        (check (bound-row-ref source 'compatibilityOverlay)
               => +opencypher-linear-result-overlay+)
        (check (string-prefix?
                "sha256:"
                (bound-row-ref source 'sourceExpressionDigest))
               => #t)
        (check (string-prefix?
                "sha256:"
                (bound-row-ref source 'replacementExpressionDigest))
               => #t))
      (for-each
       (lambda (expectation)
         (let* ((binding
                 (bound-grammar-ir-binding
                  opencypher-2024-1-bound-grammar-ir
                  'rule (car expectation)))
                (source (bound-grammar-ir-ref binding 'source)))
           (check (bound-row-ref source 'compatibilityOverlay)
                  => (cdr expectation))
           (check (string-prefix?
                   "sha256:"
                   (bound-row-ref source 'replacementExpressionDigest))
                  => #t)))
       (list
        (cons '|arithmetic unary| +opencypher-nullable-prefix-normalization+)
        (cons '|boolean factor| +opencypher-nullable-prefix-normalization+)
        (cons '|pattern source| +opencypher-nullable-prefix-normalization+)
        (cons '|list value constructor|
              +opencypher-nullable-prefix-normalization+)
        (cons '|list literal| +opencypher-nullable-prefix-normalization+)
        (cons '|general literal| +opencypher-constructor-deduplication+)
        (cons '|boolean literal| +opencypher-literal-keyword-preference+)
        (cons '|null literal| +opencypher-literal-keyword-preference+)
        (cons '|graph reference| +opencypher-qualified-reference-normalization+)
        (cons '|procedure reference|
              +opencypher-qualified-reference-normalization+)
        (cons '|function reference|
              +opencypher-qualified-reference-normalization+)))
      (let* ((binding
              (bound-grammar-ir-binding
               opencypher-2024-1-bound-grammar-ir
               'rule '|non-reserved word|))
             (source (bound-grammar-ir-ref binding 'source)))
        (check (bound-row-ref source 'precedenceOverlay)
               => +opencypher-non-reserved-word-preference+)
        (check (string-prefix?
                "sha256:"
                (bound-row-ref source 'replacementExpressionDigest))
               => #t))
      (let* ((spec (parser-ir-ref opencypher-2024-1-parser-ir 'lr-spec))
             (productions (lr-spec-ref spec 'productions))
             (actions (lr-spec-ref spec 'actions))
             (nullable-prefixes
              '("$arithmetic unary" "$boolean factor" "$pattern source"
                "$list value constructor" "$list literal"))
             (nullable-prefix-helpers
              (filter
               (lambda (production)
                 (and (null? (production-rhs production))
                      (ormap
                       (lambda (prefix)
                         (string-prefix?
                          prefix
                          (symbol->string (production-lhs production))))
                       nullable-prefixes)))
               productions))
             (fork-cells
              (apply +
                     (map
                      (lambda (row)
                        (length (filter fork-action-entry? row)))
                      (vector->list actions))))
             (initial-actions (vector-ref actions 0)))
        (check (lr-spec-ref spec 'state-count) => 1317)
        (check nullable-prefix-helpers => '())
        ;; The source-owned non-reserved-word precedence resolves 129 static
        ;; cells; remaining runtime forks retain selective-GLR admission.
        (check fork-cells => 321)
        (for-each
         (lambda (literal)
           (let (entry
                 (assoc (list 'terminal 'literal literal) initial-actions))
             (check (and entry (eq? (cadr entry) 'shift)) => #t)))
         '("UNWIND" "WITH" "RETURN"))))
    (test-case "representative and negative fixtures publish typed artifacts"
      (for-each
       (lambda (fixture)
         (let* ((source (syntax-fixture-source fixture))
                (artifact (parse-opencypher-2024-1 source))
                (expected (syntax-fixture-expected-status fixture))
                (accepted? (parse-artifact-success? artifact)))
           (check (list (syntax-fixture-id fixture) accepted?)
                  => (list (syntax-fixture-id fixture)
                           (eq? expected 'accepted)))
           (check (parse-artifact-valid? artifact) => #t)
           (when accepted?
             (let* ((root (parse-artifact->cst artifact))
                    (kinds (cst-node-kinds root)))
               (check (syntax-node-kind root)
                      => (syntax-fixture-root-kind fixture))
               (check (parse-artifact-roundtrip artifact) => source)
               (for-each
                (lambda (required-kind)
                  (check (member required-kind kinds) ? values))
                (syntax-fixture-required-kinds fixture))))))
       opencypher-2024-1-fixtures))
    ;; This checks the source-format boundary itself: openCypher admits its
    ;; native backtick/radix forms while GQL-only binary and trailing-dot forms
    ;; remain typed rejections rather than leaking through shared scanners.
    (test-case "native delimited identifiers and numeric profile remain isolated"
      (for-each
       (lambda (source)
         (let (artifact (parse-opencypher-2024-1 source))
           (check (parse-artifact-success? artifact) => #t)
           (check (parse-artifact-valid? artifact) => #t)
           (check (parse-artifact-roundtrip artifact) => source)))
       '("MATCH (`a``b`) RETURN `a``b`\n"
         "RETURN 1 AS one\n"
         "RETURN $1 AS value\n"
         "MERGE (n:L) ON CREATE SET n.x = 1 ON MATCH SET n.x = 2 RETURN n\n"
         "MERGE (a)-[r:R]-(b) RETURN r\n"
         "RETURN 1 AS n UNION ALL RETURN 2 AS n\n"
         "RETURN false = true IS NULL AS value\n"
         "UNWIND [0xFF, 0o77, .5, 1.25e+2, 3.0F] AS n RETURN n\n"))
      (for-each
       (lambda (source)
         (let (artifact (parse-opencypher-2024-1 source))
           (check (parse-artifact-success? artifact) => #f)
           (check (parse-artifact-valid? artifact) => #t)))
       '("UNWIND [0b101] AS n RETURN n\n"
         "UNWIND [1.] AS n RETURN n\n")))
    (test-case "static conflict normalization removes boolean path explosion"
      (let* ((source (repeated-boolean-list-source 300))
             (tokens
              (parser-significant-tokens
               opencypher-2024-1-parser
               (lex-source opencypher-2024-1-parser source)))
             (spec
              (bound-row-ref
               (parser-machine-ir opencypher-2024-1-parser) 'lr-spec)))
        (let-values (((_root rest receipt)
                      (lr-parse/receipt spec tokens 1)))
          (check rest => '())
          (check (bound-row-ref receipt 'branchesExplored) => 0)
          (check (bound-row-ref receipt 'speculativeBranchesExplored) => 0)
          (check (bound-row-ref receipt 'maxSpeculativeDepth) => 0)
          (check (bound-row-ref receipt 'successfulCompletions) => 1))))))
