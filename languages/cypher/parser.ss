;;; -*- Gerbil -*-
;;; Canonical public parser entry for openCypher 2024.1.

(import (only-in :gerbil-parser/language-support/entry deflanguage-parser-loader language-parser-entry-ref language-metadata-ref)
        ./grammar)
(export +opencypher-version+ +opencypher-syntax-contract+ opencypher-language-grammar (import: ./grammar)
        opencypher-language
        parse-opencypher)

(export +opencypher-commit+ +opencypher-bnf-digest+ +opencypher-linear-result-overlay+ +opencypher-numeric-parameter-overlay+ +opencypher-merge-actions-overlay+ +opencypher-undirected-merge-overlay+ +opencypher-union-all-overlay+ +opencypher-advanced-predicate-precedence-overlay+ +opencypher-qualified-reference-normalization+ +opencypher-nullable-prefix-normalization+ +opencypher-constructor-deduplication+ +opencypher-parameter-path-deduplication+ +opencypher-literal-keyword-preference+ +opencypher-non-reserved-word-preference+ +opencypher-representative-query+)

(def +opencypher-commit+
  "30b451d3b7c94ee5a84a0fdc223947a442dd9493")
(def +opencypher-bnf-digest+
  "sha256:c0b5454f001b59b401756158bf88e27847c8ace71f1abc8df1e05f8b710b9f50")
(def +opencypher-linear-result-overlay+
  '((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
    (namespace . opencypher)
    (name . linear-result-only)
    (sourceVersion . "2024.1")
    (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493")))
(def +opencypher-numeric-parameter-overlay+
  '((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
    (namespace . opencypher)
    (name . numeric-parameter)
    (sourceVersion . "2024.1")
    (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493")))
(def +opencypher-merge-actions-overlay+
  '((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
    (namespace . opencypher)
    (name . repeated-merge-actions)
    (sourceVersion . "2024.1")
    (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493")))
(def +opencypher-undirected-merge-overlay+
  '((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
    (namespace . opencypher)
    (name . undirected-merge-relationship)
    (sourceVersion . "2024.1")
    (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493")))
(def +opencypher-union-all-overlay+
  '((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
    (namespace . opencypher)
    (name . union-all)
    (sourceVersion . "2024.1")
    (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493")))
(def +opencypher-advanced-predicate-precedence-overlay+
  '((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
    (namespace . opencypher)
    (name . advanced-predicate-precedence)
    (sourceVersion . "2024.1")
    (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493")))
(def +opencypher-qualified-reference-normalization+
  '((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
    (namespace . opencypher)
    (name . qualified-reference-left-factoring)
    (kind . normalization)
    (sourceVersion . "2024.1")
    (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493")))
(def +opencypher-nullable-prefix-normalization+
  '((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
    (namespace . opencypher)
    (name . nullable-prefix-left-factoring)
    (kind . normalization)
    (sourceVersion . "2024.1")
    (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493")))
(def +opencypher-constructor-deduplication+
  '((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
    (namespace . opencypher)
    (name . value-constructor-deduplication)
    (kind . normalization)
    (sourceVersion . "2024.1")
    (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493")))
(def +opencypher-parameter-path-deduplication+
  '((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
    (namespace . opencypher)
    (name . parameter-path-deduplication)
    (kind . normalization)
    (sourceVersion . "2024.1")
    (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493")))
(def +opencypher-literal-keyword-preference+
  '((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
    (namespace . opencypher)
    (name . literal-keyword-preference)
    (kind . disambiguation)
    (sourceVersion . "2024.1")
    (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493")))
(def +opencypher-non-reserved-word-preference+
  '((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
    (namespace . opencypher)
    (name . non-reserved-word-preference)
    (kind . disambiguation)
    (sourceVersion . "2024.1")
    (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493")))
(def +opencypher-representative-query+ "MATCH (n) RETURN n\n")

(deflanguage-parser-loader opencypher-language
  (grammar opencypher-language-grammar opencypher-syntax)
  (parse parse-opencypher)
  (metadata `((language . "opencypher")
              (version . "2024.1")
              (contract . "opencypher-2024.1-syntax.v1")
              (grammar-format . iso-bnf)
              (reference-commit . ,+opencypher-commit+)
              (source-digest . ,+opencypher-bnf-digest+))))

(def +opencypher-version+ (language-metadata-ref (language-parser-entry-ref opencypher-language 'metadata) 'version))

(def +opencypher-syntax-contract+ (language-metadata-ref (language-parser-entry-ref opencypher-language 'metadata) 'contract))
