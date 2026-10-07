;;; -*- Gerbil -*-
;;; Canonical public parser entry for HCL native syntax v2.24.0.

(import (only-in :gerbil-parser/language-support/entry deflanguage-parser-loader language-parser-entry-ref language-metadata-ref)
        ./grammar)
(export +hcl-native-syntax-version+ +hcl-syntax-contract+ hcl-language-grammar (import: ./grammar)
        hcl-language
        parse-hcl)

(deflanguage-parser-loader hcl-language
  (grammar hcl-language-grammar hcl-syntax)
  (parse parse-hcl)
  (metadata `((language . "hcl")
              (version . "v2.24.0")
              (contract . "hcl-native-v2.24.0.v1")
              (grammar-format . concise-dsl)
              (reference-commit . ,+hcl-native-syntax-commit+))))

(def +hcl-native-syntax-version+ (language-metadata-ref (language-parser-entry-ref hcl-language 'metadata) 'version))

(def +hcl-syntax-contract+ (language-metadata-ref (language-parser-entry-ref hcl-language 'metadata) 'contract))
