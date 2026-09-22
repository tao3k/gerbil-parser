#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :gerbil-parser/language-support
        :gerbil-parser/languages/cypher/opencypher-2024-1/grammar
        :gerbil-parser/languages/cypher/opencypher-2024-1/source)

;; Test projections use one fail-closed lookup boundary instead of repeating
;; the source-map representation throughout behavioral assertions.
(def (alist-value fields key)
  (alet (entry (assq key fields))
    (cdr entry)))

(def (grammar-rule-expression rules name)
  (alet (entry (assq name rules))
    (cadr entry)))

(def iso-bnf-source-tests
  (test-suite "ISO WG3 BNF grammar source"
    (test-case "the complete openCypher production catalog is immutable"
      (check (iso-bnf-source-language opencypher-2024-1-bnf)
             => "opencypher")
      (check (iso-bnf-source-version opencypher-2024-1-bnf)
             => +opencypher-version+)
      (check (iso-bnf-source-commit opencypher-2024-1-bnf)
             => +opencypher-commit+)
      (check (iso-bnf-source-digest opencypher-2024-1-bnf)
             => +opencypher-bnf-digest+)
      (check (length (iso-bnf-source-productions opencypher-2024-1-bnf))
             => 352)
      (check (iso-bnf-source-production opencypher-2024-1-bnf "program")
             ? iso-bnf-production?)
      (check (iso-bnf-source-production opencypher-2024-1-bnf
                                        "value expression")
             ? iso-bnf-production?)
      (check (iso-bnf-source-production opencypher-2024-1-bnf
                                        "binary digit")
             ? iso-bnf-production?)
      (let* ((program
              (iso-bnf-source-production opencypher-2024-1-bnf "program"))
             (ast (iso-bnf-production-ast program)))
        (check (car ast) => 'choice)
        (check (iso-bnf-production-references program)
               => '("procedure specification" "standalone procedure call")))
      ;; ISO WG3 ellipsis is one-or-more. Compatibility corrections belong in
      ;; a separately receipted rule overlay, never in the notation parser.
      (check (iso-bnf-production-ast
              (iso-bnf-source-production opencypher-2024-1-bnf
                                         "linear statement"))
             => '(sequence
                  (repeat1 (reference "primitive statement"))
                  (optional (reference "primitive result statement"))))
      (check (grammar-rule-expression
              (iso-bnf-source-grammar-rules opencypher-2024-1-bnf)
              'identifier)
             => '(alias identifier
                  (choice (reference |regular identifier|)
                          (reference |delimited identifier|)
                          (reference |non-reserved word|))))
      (check (grammar-rule-expression
              (iso-bnf-source-grammar-rules opencypher-2024-1-bnf)
              '|delimited identifier|)
             => '(alias |delimited identifier|
                  (reference |accent quoted character sequence|)))
      (check (grammar-rule-expression
              (iso-bnf-source-grammar-rules opencypher-2024-1-bnf)
              '|accent quoted character sequence|)
             => '(alias |accent quoted character sequence|
                  (token delimited-identifier)))
      (check
       (grammar-rule-expression
        (iso-bnf-source-grammar-rules opencypher-2024-1-bnf)
        '|create node pattern filler|)
       => '(alias |create node pattern filler|
            (choice
             (sequence
              (reference |binding variable|)
              (optional
               (reference |create node label and property set specification|)))
             (reference |create node label and property set specification|))))
      (check
       (grammar-rule-expression
        (iso-bnf-source-grammar-rules opencypher-2024-1-bnf)
        '|create node label and property set specification|)
       => '(alias |create node label and property set specification|
            (choice
             (sequence
              (reference |create node label set specification|)
              (optional
               (reference |create element property specification|)))
             (reference |create element property specification|))))
      (for-each
       (lambda (production)
         (check (pair? (iso-bnf-production-ast production)) => #t))
       (iso-bnf-source-productions opencypher-2024-1-bnf)))
    (test-case "unresolved production references fail closed"
      (check-exception
       (parse-iso-bnf-source
        "example" "v1" "commit"
        "<program> ::= <missing production>\n")
       true))
    (test-case "duplicate production identities fail closed"
      (check-exception
       (parse-iso-bnf-source
        "example" "v1" "commit"
        "<program> ::= OK\n<program> ::= ALSO_OK\n")
       true))
    (test-case "declared source digest mismatch fails closed"
      (check-exception
       (parse-iso-bnf-source/expected
        "example" "v1" "commit" "sha256:not-the-source"
       "<program> ::= OK\n")
       true))
    (test-case "rule overlays bind the exact source expression and receipt"
      (let* ((source (parse-iso-bnf-source
                      "example" "v1" "commit"
                      "<program> ::= ITEM...\n"))
             (expected '(alias program (repeat1 (literal "ITEM"))))
             (replacement '(alias program (repeat (literal "ITEM"))))
             (metadata
              `((schema . ,+iso-bnf-rule-overlay-schema+)
                (namespace . example)
                (name . zero-or-more)
                (sourceVersion . "v1")
                (upstreamCommit . "commit")))
             (overrides
              (list (list 'program metadata
                          expected replacement)))
             (rules
              (iso-bnf-source-grammar-rules/overrides source overrides))
             (source-map
              (iso-bnf-source-declaration-sources/overrides
               source "example.bnf" overrides))
             (source-metadata
              (alist-value (alist-value source-map 'rule) 'program)))
        (check (grammar-rule-expression rules 'program) => replacement)
        (check (alist-value source-metadata 'compatibilityOverlay)
               => metadata)
        (check (string-prefix? "sha256:"
                               (alist-value source-metadata
                                            'sourceExpressionDigest))
               => #t)
        (check (string-prefix? "sha256:"
                               (alist-value source-metadata
                                            'replacementExpressionDigest))
               => #t)
        (check-exception
         (iso-bnf-source-grammar-rules/overrides
          source
          (list (list 'program metadata replacement expected)))
         true)
        (check-exception
         (iso-bnf-source-grammar-rules/overrides
          source
          (list (list 'missing metadata expected replacement)))
         true)))))

(export iso-bnf-source-tests)
