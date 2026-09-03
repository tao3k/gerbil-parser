;;; -*- Gerbil -*-
;;; Canonical public parser entry for the arithmetic v1 reference language.

(import :gerbil-parser/src/language/entry
        ./grammar)
(export (import: ./grammar)
        arithmetic-v1-language
        parse-arithmetic-v1)

(deflanguage-parser arithmetic-v1-language
  (grammar arithmetic-language-grammar)
  (parse parse-arithmetic-v1))
