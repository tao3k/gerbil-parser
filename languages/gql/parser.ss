;;; -*- Gerbil -*-
;;; Canonical public parser entry for ISO/IEC 39075:2024 GQL.

(import (only-in :gerbil-parser/language-support/entry deflanguage-parser-loader language-parser-entry-ref language-metadata-ref)
        ./grammar)
(export +gql-standard-edition+ +gql-syntax-contract+ gql-language-grammar (import: ./grammar)
        gql-language
        parse-gql)

(deflanguage-parser-loader gql-language
  (grammar gql-language-grammar gql-syntax)
  (parse parse-gql)
  (metadata `((language . "gql")
              (version . "edition-1-2024-04")
              (contract . "iso-iec-39075-2024.opengql-1.9.0-syntax.v1")
              (grammar-format . antlr4)
              (reference-version . ,+gql-opengql-reference-version+)
              (reference-commit . ,+gql-opengql-reference-commit+)
              (source-digest . ,+gql-antlr4-digest+))))

(def +gql-standard-edition+ (language-metadata-ref (language-parser-entry-ref gql-language 'metadata) 'version))

(def +gql-syntax-contract+ (language-metadata-ref (language-parser-entry-ref gql-language 'metadata) 'contract))
