;;; -*- Gerbil -*-
;;; Canonical public parser entry for ISO/IEC 39075:2024 GQL.

(import (only-in ./fixtures gql-iso-official-fixtures)
        (only-in :gerbil-parser/src/language/entry check-language-loader-fixtures!)
        (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-support deflanguage-parser-loader LanguageLoader.)
        ./grammar)
(export (import: ./grammar)
        gql-iso-39075-2024-language
        parse-gql-iso-39075-2024)

(deflanguage-parser-loader (gql-iso-39075-2024-language :: self LanguageLoader.)
  (grammar gql-iso-language-grammar)
  (parse parse-gql-iso-39075-2024)
  (slots metadata: (.o grammar-format: 'antlr4)
         fixtures: (lambda () gql-iso-official-fixtures)
         tests: (list (cons 'fixtures check-language-loader-fixtures!))))
