;;; -*- Gerbil -*-
(import :gerbil-parser/src/language/entry ./grammar)
(export (import: ./grammar) tla-plus-1-5-language parse-tla-plus-1-5)
(deflanguage-parser tla-plus-1-5-language
  (grammar tla-plus-1-5-language-grammar)
  (parse parse-tla-plus-1-5))
