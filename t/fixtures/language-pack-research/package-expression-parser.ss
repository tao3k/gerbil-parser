;;; -*- Gerbil -*-
;;; Complete research package entry using the existing checked Loader.
(import (only-in :gerbil-parser/language-support/development
                 deflanguage-development-loader LanguageDevelopmentLoader.)
        "package-expression-grammar")
(export list-study-language parse-list-study)

(deflanguage-development-loader (list-study-language :: self LanguageDevelopmentLoader.)
  (grammar list-study-language-grammar)
  (parse parse-list-study))
