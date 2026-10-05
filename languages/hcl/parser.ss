;;; -*- Gerbil -*-
;;; Canonical public parser entry for HCL native syntax v2.24.0.

(import (only-in ./fixtures hcl-official-fixtures)
        (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/src/language/entry deflanguage-parser-loader LanguageLoader.)
        ./grammar)
(export (import: ./grammar)
        hcl-language
        parse-hcl)

(deflanguage-parser-loader (hcl-language :: self LanguageLoader.)
  (grammar hcl-language-grammar)
  (parse parse-hcl)
  (slots metadata: (.o grammar-format: 'concise-dsl reference-commit: +hcl-native-syntax-commit+)
         fixtures: hcl-official-fixtures))
