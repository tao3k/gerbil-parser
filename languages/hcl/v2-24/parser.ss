;;; -*- Gerbil -*-
;;; Canonical public parser entry for HCL native syntax v2.24.0.

(import (only-in :gerbil-parser/language-support deflanguage-loader)
        ./grammar)
(export (import: ./grammar)
        hcl-v2-24-language
        parse-hcl-v2-24)

(deflanguage-loader hcl-v2-24-language
  (grammar hcl-v2-24-language-grammar)
  (parse parse-hcl-v2-24))
