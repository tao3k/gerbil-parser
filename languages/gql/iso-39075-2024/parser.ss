;;; -*- Gerbil -*-
;;; Canonical public parser entry for ISO/IEC 39075:2024 GQL.

(import :gerbil-parser/src/language/entry
        ./grammar)
(export (import: ./grammar)
        gql-iso-39075-2024-language
        parse-gql-iso-39075-2024)

(deflanguage-parser gql-iso-39075-2024-language
  (grammar gql-iso-language-grammar)
  (parse parse-gql-iso-39075-2024))
