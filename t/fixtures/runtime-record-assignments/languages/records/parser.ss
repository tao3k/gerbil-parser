;;; -*- Gerbil -*-
;;; Record assignments parser entry built only from the installed facade.

(import (only-in :gerbil-parser/language-support deflanguage-parser-loader)
        (only-in ./grammar records-language-grammar))
(export records-language parse-records)

(deflanguage-parser-loader records-language
  (grammar records-language-grammar)
  (parse parse-records))
