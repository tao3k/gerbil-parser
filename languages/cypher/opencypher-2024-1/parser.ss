;;; -*- Gerbil -*-
;;; Canonical public parser entry for openCypher 2024.1.

(import (only-in ./fixtures opencypher-2024-1-fixtures)
        (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/src/language/entry deflanguage-parser-loader LanguageLoader.)
        ./grammar)
(export (import: ./grammar)
        opencypher-2024-1-language
        parse-opencypher-2024-1)

(deflanguage-parser-loader (opencypher-2024-1-language :: self LanguageLoader.)
  (grammar opencypher-2024-1-language-grammar)
  (parse parse-opencypher-2024-1)
  (slots metadata: (.o grammar-format: 'iso-bnf)
         fixtures: (lambda () opencypher-2024-1-fixtures)))
