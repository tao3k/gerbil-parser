;;; -*- Gerbil -*-
;;; Record assignments parser entry built only from the installed facade.

(import (only-in :gerbil-parser/language-support deflanguage-parser-loader)
        (only-in ./grammar records-language-grammar))
(export records-v1-language parse-records-v1)

(deflanguage-parser-loader records-v1-language
  (grammar records-language-grammar)
  (parse parse-records-v1))
