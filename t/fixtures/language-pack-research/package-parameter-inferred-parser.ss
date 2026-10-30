;;; -*- Gerbil -*-
(import (only-in :gerbil-parser/language-support/development deflanguage-development-loader)
        "package-parameter-inferred-grammar")
(export list-parameter-inferred-language parse-list-parameter-inferred)
(deflanguage-development-loader list-parameter-inferred-language
  (grammar list-parameter-inferred-language-grammar)
  (parse parse-list-parameter-inferred))
