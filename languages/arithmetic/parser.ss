;;; -*- Gerbil -*-
;;; Canonical public parser entry for the arithmetic v1 reference language.

(import (only-in ./fixtures arithmetic-basic-fixture)
        (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/src/language/entry deflanguage-parser-loader LanguageLoader.)
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
        arithmetic-language
        parse-arithmetic)

(deflanguage-parser-loader (arithmetic-language :: self LanguageLoader.)
  (grammar arithmetic-language-grammar)
  (parse parse-arithmetic)
  (slots metadata: (.o grammar-format: 'concise-dsl)
         fixtures: (list arithmetic-basic-fixture)))
