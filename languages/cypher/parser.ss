;;; -*- Gerbil -*-
;;; Canonical public parser entry for openCypher 2024.1.

(import (only-in :gerbil-parser/language-support/entry deflanguage-parser-loader language-parser-entry-ref language-metadata-ref)
        ./grammar)
(export +opencypher-version+ +opencypher-syntax-contract+ opencypher-language-grammar (import: ./grammar)
        opencypher-language
        parse-opencypher)

(export +opencypher-commit+ +opencypher-bnf-digest+ +opencypher-representative-query+)

(def +opencypher-commit+
  "30b451d3b7c94ee5a84a0fdc223947a442dd9493")
(def +opencypher-bnf-digest+
  "sha256:c0b5454f001b59b401756158bf88e27847c8ace71f1abc8df1e05f8b710b9f50")
(def +opencypher-representative-query+ "MATCH (n) RETURN n\n")

(deflanguage-parser-loader opencypher-language
  (grammar opencypher-syntax)
  (parse parse-opencypher)
  (slots
    source-schema: `((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
                     (namespace . opencypher)
                     (sourceVersion . "2024.1")
                     (upstreamCommit . ,+opencypher-commit+))
    metadata: `((language . "opencypher")
                (version . "2024.1")
                (contract . "opencypher-2024.1-syntax.v1")
                (grammar-format . iso-bnf)
                (reference-commit . ,+opencypher-commit+)
                (source-digest . ,+opencypher-bnf-digest+))))

(def opencypher-language-grammar (language-parser-entry-ref opencypher-language 'descriptor))

(def +opencypher-version+ (language-metadata-ref (language-parser-entry-ref opencypher-language 'metadata) 'version))

(def +opencypher-syntax-contract+ (language-metadata-ref (language-parser-entry-ref opencypher-language 'metadata) 'contract))
