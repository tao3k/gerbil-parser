#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-

(import :std/test
        :gerbil-parser/language-support
        (only-in :gerbil-parser/language-support/iso-bnf
                 iso-bnf-source->datum iso-bnf-source-from-datum)
        :gerbil-parser/languages/cypher/parser)

;; Test projections use one fail-closed lookup boundary instead of repeating
;; the source-map representation throughout behavioral assertions.
(def (alist-value fields key)
  (alet (entry (assq key fields))
    (cdr entry)))

(def (grammar-rule-expression rules name)
  (alet (entry (assq name rules))
    (cadr entry)))

(deflanguage-parser-tests iso-bnf-source-tests "ISO WG3 BNF grammar source"
  (engine)
    (scenario "frozen source catalogs retain all production fields"
      (sequential ((data (iso-bnf-source->datum opencypher-bnf))
             (copy (iso-bnf-source-from-datum data)))
        (equal (iso-bnf-source->datum copy)  data)
        (equal (iso-bnf-source-production copy "program")
                (iso-bnf-source-production opencypher-bnf "program")))
      (raises (iso-bnf-source-from-datum '(invalid)) true)
      (raises
       (iso-bnf-source-from-datum
        (list +iso-bnf-source-schema+ "test" "v1" "commit" "digest" '(bad-row))) true))
    (scenario "the complete openCypher production catalog is immutable"
      (equal (iso-bnf-source-language opencypher-bnf)
              "opencypher")
      (equal (iso-bnf-source-version opencypher-bnf)
              +opencypher-version+)
      (equal (iso-bnf-source-commit opencypher-bnf)
              +opencypher-commit+)
      (equal (iso-bnf-source-digest opencypher-bnf)
              +opencypher-bnf-digest+)
      (equal (length (iso-bnf-source-productions opencypher-bnf))
              352)
      (matches (iso-bnf-source-production opencypher-bnf "program")
              iso-bnf-production?)
      (matches (iso-bnf-source-production opencypher-bnf
                                        "value expression")
              iso-bnf-production?)
      (matches (iso-bnf-source-production opencypher-bnf
                                        "binary digit")
              iso-bnf-production?)
      (sequential ((program
              (iso-bnf-source-production opencypher-bnf "program"))
             (ast (iso-bnf-production-ast program)))
        (equal (car ast)  'choice)
        (equal (iso-bnf-production-references program)
                '("procedure specification" "standalone procedure call")))
      ;; ISO WG3 ellipsis is one-or-more. Compatibility corrections belong in
      ;; a separately receipted rule overlay, never in the notation parser.
      (equal (iso-bnf-production-ast
              (iso-bnf-source-production opencypher-bnf
                                         "linear statement"))
              '(sequence
                  (repeat1 (reference "primitive statement"))
                  (optional (reference "primitive result statement"))))
      (equal (grammar-rule-expression
              (iso-bnf-source-grammar-rules opencypher-bnf)
              'identifier)
              '(alias identifier
                  (choice (reference |regular identifier|)
                          (reference |delimited identifier|)
                          (reference |non-reserved word|))))
      (equal (grammar-rule-expression
              (iso-bnf-source-grammar-rules opencypher-bnf)
              '|delimited identifier|)
              '(alias |delimited identifier|
                  (reference |accent quoted character sequence|)))
      (equal (grammar-rule-expression
              (iso-bnf-source-grammar-rules opencypher-bnf)
              '|accent quoted character sequence|)
              '(alias |accent quoted character sequence|
                  (token delimited-identifier)))
      (equal
       (grammar-rule-expression
        (iso-bnf-source-grammar-rules opencypher-bnf)
        '|create node pattern filler|)
        '(alias |create node pattern filler|
            (choice
             (sequence
              (reference |binding variable|)
              (optional
               (reference |create node label and property set specification|)))
             (reference |create node label and property set specification|))))
      (equal
       (grammar-rule-expression
        (iso-bnf-source-grammar-rules opencypher-bnf)
        '|create node label and property set specification|)
        '(alias |create node label and property set specification|
            (choice
             (sequence
              (reference |create node label set specification|)
              (optional
               (reference |create element property specification|)))
             (reference |create element property specification|))))
      (each (production) ((iso-bnf-source-productions opencypher-bnf))
         (equal (pair? (iso-bnf-production-ast production))  #t)))
    (scenario "unresolved production references fail closed"
      (raises
       (parse-iso-bnf-source
        "example" "v1" "commit"
        "<program> ::= <missing production>\n")
       true))
    (scenario "duplicate production identities fail closed"
      (raises
       (parse-iso-bnf-source
        "example" "v1" "commit"
        "<program> ::= OK\n<program> ::= ALSO_OK\n")
       true))
    (scenario "declared source digest mismatch fails closed"
      (raises
       (parse-iso-bnf-source/expected
        "example" "v1" "commit" "sha256:not-the-source"
       "<program> ::= OK\n")
       true))
    (scenario "rule overlays bind the exact source expression and receipt"
      (sequential ((source (parse-iso-bnf-source
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
        (equal (grammar-rule-expression rules 'program)  replacement)
        (equal (alist-value source-metadata 'compatibilityOverlay)
                metadata)
        (equal (string-prefix? "sha256:"
                               (alist-value source-metadata
                                            'sourceExpressionDigest))
                #t)
        (equal (string-prefix? "sha256:"
                               (alist-value source-metadata
                                            'replacementExpressionDigest))
                #t)
        (raises
         (iso-bnf-source-grammar-rules/overrides
          source
          (list (list 'program metadata replacement expected)))
         true)
        (raises
         (iso-bnf-source-grammar-rules/overrides
          source
          (list (list 'missing metadata expected replacement)))
         true))))


;; gxtest discovers only exported names ending in -test.
(def iso-bnf-source-test iso-bnf-source-tests)
(export iso-bnf-source-test)
