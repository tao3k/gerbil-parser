;;; -*- Gerbil -*-
;;; Parser names describe syntax scope; grammar metadata owns version identity.

(import (only-in :gerbil-parser/language-support/fixture defsyntax-corpus)
        (only-in ./fixtures tla-plus-core-fixtures)
        (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/src/language/entry deflanguage-parser-loader LanguageLoader.)
        (only-in ./grammars/core
                 tla-plus-core-language-grammar tla-plus-core-parser)
        (only-in ./grammars/layout
                 tla-plus-layout-language-grammar tla-plus-layout-parser))
(export tla-plus-core-language tla-plus-layout-language
        tla-plus-core-parser tla-plus-layout-parser
        parse-tla-plus-core parse-tla-plus-layout parse-tla-plus)

(deflanguage-parser-loader (tla-plus-core-language :: self LanguageLoader.)
  (grammar tla-plus-core-language-grammar)
  (parse parse-tla-plus-core)
  (slots metadata: (.o grammar-format: 'concise-dsl)
         fixtures: (lambda () tla-plus-core-fixtures)))

(defsyntax-corpus tla-plus-layout-fixtures
  (identity "tla-plus" "v2" "tla-plus.native-layout.v2")
  (accepted
   ("tla-plus/layout/aligned" tla-layout-aligned (text "---- MODULE J ----\nInit ==\n  /\\ TRUE\n  /\\ FALSE\n====\n") SourceFile (JunctionExpression)))
  (rejected
   ("tla-plus/layout/incomplete" tla-layout-incomplete (text "---- MODULE J ----\nInit ==\n  /\\\n====\n"))))

(deflanguage-parser-loader (tla-plus-layout-language :: self LanguageLoader.)
  (grammar tla-plus-layout-language-grammar)
  (parse parse-tla-plus-layout)
  (slots metadata: (.o grammar-format: 'concise-dsl)
         fixtures: (lambda () tla-plus-layout-fixtures)))

(def parse-tla-plus parse-tla-plus-layout)
