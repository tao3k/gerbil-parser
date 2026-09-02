;;; -*- Gerbil -*-
;;; Canonical public parser entry for the arithmetic v1 reference language.

(import :gerbil-parser/src/language/entry
        ./grammar)
(export (import: ./grammar)
        arithmetic-v1-language
        parse-arithmetic-v1)

(deflanguage-parser-entry arithmetic-v1-language
  (identity "arithmetic" "v1" "arithmetic-expression.v1")
  (machine arithmetic-parser)
  (parse parse-arithmetic-v1))
