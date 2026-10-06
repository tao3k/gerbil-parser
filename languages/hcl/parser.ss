;;; -*- Gerbil -*-
;;; Canonical public parser entry for HCL native syntax v2.24.0.

(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-build-support
                 declare-language-build-strategy make-fused-reduction-strategy make-rust-rowan-strategy)
        (only-in :gerbil-parser/language-support/development deflanguage-development-loader LanguageDevelopmentLoader.)
        ./grammar
        (only-in :gerbil-parser/src/compiler/hcl-source direct-parse-hcl direct-lex-hcl direct-hcl-grammar-digest))
(export (import: ./grammar)
        hcl-language
        parse-hcl)

(deflanguage-development-loader (hcl-language :: self LanguageDevelopmentLoader.)
  (grammar hcl-language-grammar)
  (parse parse-hcl)
  (slots metadata: (.o grammar-format: 'concise-dsl reference-commit: +hcl-native-syntax-commit+)
         build-strategies: (list (declare-language-build-strategy 'fused-reductions
                                   (make-fused-reduction-strategy hcl-language-grammar))
                                 (declare-language-build-strategy 'rust-rowan
                                   (make-rust-rowan-strategy hcl-language-grammar)))
         fixtures: hcl-official-fixtures
         native-test-profile: (.o profile: 'recursive-source
                                  digest: direct-hcl-grammar-digest
                                  source: direct-parse-hcl lexer: direct-lex-hcl)))
