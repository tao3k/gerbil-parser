;;; -*- Gerbil -*-
;;; Canonical public parser entry for HCL native syntax v2.24.0.

(import :gerbil-parser/src/language/entry
        ./grammar)
(export (import: ./grammar)
        hcl-v2-24-language
        parse-hcl-v2-24)

(deflanguage-parser hcl-v2-24-language
  (grammar hcl-v2-24-language-grammar)
  (parse parse-hcl-v2-24))
