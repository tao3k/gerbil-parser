;;; -*- Gerbil -*-
;;; Canonical public parser entry for openCypher 2024.1.

(import :gerbil-parser/src/language/entry
        ./grammar)
(export (import: ./grammar)
        opencypher-2024-1-language
        parse-opencypher-2024-1)

(deflanguage-parser opencypher-2024-1-language
  (grammar opencypher-2024-1-language-grammar)
  (parse parse-opencypher-2024-1))
