;;; -*- Gerbil -*-
;;; Canonical public parser entry for the arithmetic v1 reference language.

(import (only-in ./fixtures arithmetic-v1-basic-fixture)
        (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-support deflanguage-parser-loader LanguageLoader.
                 check-language-loader-fixtures!)
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

(deflanguage-parser-loader (arithmetic-v1-language :: self LanguageLoader.)
  (grammar arithmetic-language-grammar)
  (parse parse-arithmetic-v1)
  (slots metadata: (.o grammar-format: 'concise-dsl)
         fixtures: (lambda () (list arithmetic-v1-basic-fixture))
         tests: (list (cons 'fixtures check-language-loader-fixtures!))))
