;;; -*- Gerbil -*-
;;; Canonical public parser entry for the arithmetic v1 reference language.

(import (only-in :gerbil-parser/language-support deflanguage-parser-loader)
        (only-in ./grammar
                 +arithmetic-language-version+
                 +arithmetic-syntax-contract+
                 arithmetic-language-grammar
                 arithmetic-grammar
                 arithmetic-parser-ir
                 arithmetic-parser))
(export +arithmetic-language-version+
        +arithmetic-syntax-contract+
        arithmetic-language-grammar
        arithmetic-grammar
        arithmetic-parser-ir
        arithmetic-parser
        arithmetic-v1-language
        parse-arithmetic-v1)

(deflanguage-parser-loader arithmetic-v1-language
  (grammar arithmetic-language-grammar)
  (parse parse-arithmetic-v1))
