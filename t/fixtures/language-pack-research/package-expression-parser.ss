;;; -*- Gerbil -*-
;;; Complete research package entry using the existing checked Loader.
(import (only-in :gerbil-parser/src/language/entry
                 deflanguage-parser-loader LanguageLoader.)
        "package-expression-grammar")
(export list-study-language parse-list-study)

(deflanguage-parser-loader (list-study-language :: self LanguageLoader.)
  (grammar list-study-language-grammar)
  (parse parse-list-study))
