;;; -*- Gerbil -*-
;;; Canonical public parser entry for HCL native syntax v2.24.0.

(import :gerbil-parser/src/language/entry
        ./grammar)
(export (import: ./grammar)
        hcl-v2-24-language
        parse-hcl-v2-24)

(deflanguage-parser-entry hcl-v2-24-language
  (identity "hcl" +hcl-native-syntax-version+ +hcl-syntax-contract-v1+)
  (machine hcl-v2-24-parser-machine)
  (parse parse-hcl-v2-24))
