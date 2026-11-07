(import  (only-in :gerbil-parser/src/compiler/hcl-reductions direct-step direct-event-step direct-grammar-digest)
 (only-in :gerbil-parser/src/compiler/hcl-source direct-parse-hcl direct-hcl-grammar-digest))
;;; -*- Gerbil -*-
;;; Canonical public parser entry for HCL native syntax v2.24.0.

(import (only-in :gerbil-parser/language-support/entry deflanguage-parser-loader language-parser-entry-ref language-metadata-ref)
        ./grammar)
(export +hcl-native-syntax-version+ +hcl-syntax-contract+ hcl-language-grammar (import: ./grammar)
        hcl-language
        parse-hcl)

(export +hcl-native-syntax-commit+)

(def +hcl-native-syntax-commit+
     "6b5068090eef06b1f127f61529db5ba0be7ed343")

(deflanguage-parser-loader hcl-language
  (grammar hcl-syntax)
  (parse parse-hcl)
  (slots metadata: `((language . "hcl")
              (version . "v2.24.0")
              (contract . "hcl-native-v2.24.0.v1")
              (grammar-format . concise-dsl)
              (reference-commit . ,+hcl-native-syntax-commit+)))
  (backends hcl-parser
    (step direct-grammar-digest direct-step)
    (source direct-hcl-grammar-digest direct-parse-hcl)
    (event-step direct-grammar-digest direct-event-step)))

(def hcl-language-grammar (language-parser-entry-ref hcl-language 'descriptor))

(def +hcl-native-syntax-version+ (language-metadata-ref (language-parser-entry-ref hcl-language 'metadata) 'version))

(def +hcl-syntax-contract+ (language-metadata-ref (language-parser-entry-ref hcl-language 'metadata) 'contract))
