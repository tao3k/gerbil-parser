;;; -*- Gerbil -*-
;;; Parser names describe syntax scope; grammar metadata owns version identity.

(import (only-in :gerbil-parser/language-support deflanguage-parser-loader)
        (only-in ./grammars/core
                 tla-plus-core-language-grammar tla-plus-core-parser)
        (only-in ./grammars/layout
                 tla-plus-layout-language-grammar tla-plus-layout-parser))
(export tla-plus-core-language tla-plus-layout-language
        tla-plus-core-parser tla-plus-layout-parser
        parse-tla-plus-core parse-tla-plus-layout parse-tla-plus)

(deflanguage-parser-loader tla-plus-core-language
  (grammar tla-plus-core-language-grammar)
  (parse parse-tla-plus-core))

(deflanguage-parser-loader tla-plus-layout-language
  (grammar tla-plus-layout-language-grammar)
  (parse parse-tla-plus-layout))

(def parse-tla-plus parse-tla-plus-layout)
