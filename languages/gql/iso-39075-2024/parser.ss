;;; -*- Gerbil -*-
;;; Canonical public parser entry for ISO/IEC 39075:2024 GQL.

(import :gerbil-parser/src/language/entry
        ./grammar)
(export (import: ./grammar)
        gql-iso-39075-2024-language
        parse-gql-iso-39075-2024)

(deflanguage-parser-entry gql-iso-39075-2024-language
  (identity "gql" +gql-standard-edition+ +gql-syntax-contract-v1+)
  (machine gql-iso-parser)
  (parse parse-gql-iso-39075-2024))
