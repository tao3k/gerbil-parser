;;; -*- Gerbil -*-
(import (only-in :gerbil-parser/src/language/entry deflanguage-parser-loader)
        "package-parameter-retained-grammar")
(export list-parameter-retained-language parse-list-parameter-retained)
(deflanguage-parser-loader list-parameter-retained-language
  (grammar list-parameter-retained-language-grammar)
  (parse parse-list-parameter-retained))
