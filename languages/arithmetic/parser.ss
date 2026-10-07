;;; -*- Gerbil -*-
;;; Canonical public parser entry for the arithmetic v1 reference language.

(import (only-in :gerbil-parser/language-support/entry deflanguage-parser-loader language-parser-entry-ref language-metadata-ref)
        ./grammar)
(export +arithmetic-language-version+ +arithmetic-syntax-contract+ (import: ./grammar) arithmetic-language-grammar arithmetic-language parse-arithmetic)

(deflanguage-parser-loader arithmetic-language
  (grammar arithmetic-language-grammar arithmetic-syntax)
  (parse parse-arithmetic)
  (metadata '((language . "arithmetic")
              (version . "v1")
              (contract . "arithmetic-expression.v1")
              (grammar-format . concise-dsl))))

(def +arithmetic-language-version+ (language-metadata-ref (language-parser-entry-ref arithmetic-language 'metadata) 'version))

(def +arithmetic-syntax-contract+ (language-metadata-ref (language-parser-entry-ref arithmetic-language 'metadata) 'contract))
