;;; -*- Gerbil -*-
(import (only-in :gerbil-parser/language-support/development deflanguage-development-loader)
        "package-parameter-retained-grammar")
(export list-parameter-retained-language parse-list-parameter-retained)
(deflanguage-development-loader list-parameter-retained-language
  (grammar list-parameter-retained-language-grammar)
  (parse parse-list-parameter-retained))
