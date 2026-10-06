;;; -*- Gerbil -*-
;;; Canonical public parser entry for openCypher 2024.1.

(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-support/entry deflanguage-parser-loader LanguageLoader.)
        ./grammar)
(export (import: ./grammar)
        opencypher-language
        parse-opencypher)

(deflanguage-parser-loader (opencypher-language :: self LanguageLoader.)
  (grammar opencypher-language-grammar)
  (parse parse-opencypher)
  (slots metadata: (.o grammar-format: 'iso-bnf reference-commit: +opencypher-commit+
                             source-digest: +opencypher-bnf-digest+)))
