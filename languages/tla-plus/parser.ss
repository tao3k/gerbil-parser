;;; -*- Gerbil -*-
;;; Parser names describe syntax scope; grammar metadata owns version identity.

(import (only-in :gerbil-parser/language-support/fixture defsyntax-corpus)
        ./fixtures
        ./source
        ./sany-candidate
        :gerbil-parser/src/language/tlc
        (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/src/language/entry deflanguage-parser-loader LanguageLoader.)
        (only-in ./grammars/core
                 tla-plus-core-language-grammar tla-plus-core-parser)
        (only-in ./grammars/layout
                 tla-plus-layout-language-grammar tla-plus-layout-parser))
(export (import: ./fixtures) (import: ./source) (import: ./sany-candidate)
        +tla-plus-model-qualification-schema+ tla-plus-model-receipt?
        tla-plus-model-receipt-admitted tla-plus-model-receipt-output tla-plus-model-receipt->alist
        qualify-tla-plus-model qualify-tla-plus-core-model
        tla-plus-core-language tla-plus-layout-language
        tla-plus-core-parser tla-plus-layout-parser
        parse-tla-plus-core parse-tla-plus-layout parse-tla-plus)

(deflanguage-parser-loader (tla-plus-core-language :: self LanguageLoader.)
  (grammar tla-plus-core-language-grammar)
  (parse parse-tla-plus-core)
  (slots metadata: (.o grammar-format: 'concise-dsl)
         fixtures: tla-plus-core-fixtures))

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
         fixtures: tla-plus-layout-fixtures))

(def parse-tla-plus parse-tla-plus-layout)

(def (qualify-tla-plus-model spec-path config-path tlc: (tlc "tlc") workers: (workers 1))
  (qualify-language-tlc-model tla-plus-layout-language-grammar spec-path config-path tlc: tlc workers: workers))
(def (qualify-tla-plus-core-model spec-path config-path tlc: (tlc "tlc") workers: (workers 1))
  (qualify-language-tlc-model tla-plus-core-language-grammar spec-path config-path tlc: tlc workers: workers))
