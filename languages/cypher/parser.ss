;;; -*- Gerbil -*-
;;; Canonical public parser entry for openCypher 2024.1.

(import (only-in :gerbil-parser/language-support/entry deflanguage-parser-loader language-parser-entry-ref language-metadata-ref)
        ./grammar)
(export +opencypher-version+ +opencypher-syntax-contract+ opencypher-language-grammar (import: ./grammar)
        opencypher-language
        parse-opencypher)

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
