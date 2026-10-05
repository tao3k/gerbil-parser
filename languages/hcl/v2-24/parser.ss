;;; -*- Gerbil -*-
;;; Canonical public parser entry for HCL native syntax v2.24.0.

(import (only-in ./fixtures hcl-v2-24-official-fixtures)
        (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/src/language/entry deflanguage-parser-loader LanguageLoader.)
        ./grammar)
(export (import: ./grammar)
        hcl-v2-24-language
        parse-hcl-v2-24)

(deflanguage-parser-loader (hcl-v2-24-language :: self LanguageLoader.)
  (grammar hcl-v2-24-language-grammar)
  (parse parse-hcl-v2-24)
  (slots metadata: (.o grammar-format: 'concise-dsl)
         fixtures: (lambda () hcl-v2-24-official-fixtures)))
