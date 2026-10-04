;;; -*- Gerbil -*-
;;; Canonical public parser entry for openCypher 2024.1.

(import (only-in :gerbil-parser/language-support deflanguage-parser-loader)
        ./grammar)
(export (import: ./grammar)
        opencypher-2024-1-language
        parse-opencypher-2024-1)

(deflanguage-parser-loader opencypher-2024-1-language
  (grammar opencypher-2024-1-language-grammar)
  (parse parse-opencypher-2024-1))
