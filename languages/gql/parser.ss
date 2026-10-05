;;; -*- Gerbil -*-
;;; Canonical public parser entry for ISO/IEC 39075:2024 GQL.

(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/src/language/entry deflanguage-parser-loader LanguageLoader.)
        ./grammar)
(export (import: ./grammar)
        gql-language
        parse-gql)

(deflanguage-parser-loader (gql-language :: self LanguageLoader.)
  (grammar gql-language-grammar)
  (parse parse-gql)
  (slots metadata: (.o grammar-format: 'antlr4 reference-version: +gql-opengql-reference-version+
                             reference-commit: +gql-opengql-reference-commit+
                             source-digest: +gql-antlr4-digest+)
         fixtures: gql-official-fixtures))
