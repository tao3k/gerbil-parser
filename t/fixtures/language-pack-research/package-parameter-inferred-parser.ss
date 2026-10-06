;;; -*- Gerbil -*-
(import (only-in :gerbil-parser/src/language/entry deflanguage-parser-loader)
        "package-parameter-inferred-grammar")
(export list-parameter-inferred-language parse-list-parameter-inferred)
(deflanguage-parser-loader list-parameter-inferred-language
  (grammar list-parameter-inferred-language-grammar)
  (parse parse-list-parameter-inferred))
