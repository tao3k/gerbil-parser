;;; -*- Gerbil -*-
;;; Canonical public parser entry for ISO/IEC 39075:2024 GQL.

(import (only-in :gerbil-parser/language-support deflanguage-loader)
        ./grammar)
(export (import: ./grammar)
        gql-iso-39075-2024-language
        parse-gql-iso-39075-2024)

(deflanguage-loader gql-iso-39075-2024-language
  (grammar gql-iso-language-grammar)
  (parse parse-gql-iso-39075-2024))
