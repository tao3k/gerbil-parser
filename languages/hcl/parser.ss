;;; -*- Gerbil -*-
;;; Canonical public parser entry for HCL native syntax v2.24.0.

(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-build-support
                 declare-language-fused-reductions declare-language-build-strategy make-rust-rowan-strategy)
        (only-in :gerbil-parser/src/language/entry deflanguage-parser-loader LanguageLoader.)
        ./grammar
        (only-in ./direct-recursive direct-parse-hcl direct-lex-hcl direct-hcl-grammar-digest))
(export (import: ./grammar)
        hcl-language
        parse-hcl)

(deflanguage-parser-loader (hcl-language :: self LanguageLoader.)
  (grammar hcl-language-grammar)
  (parse parse-hcl)
  (slots metadata: (.o grammar-format: 'concise-dsl reference-commit: +hcl-native-syntax-commit+)
         build-strategies: (list (declare-language-fused-reductions hcl-language-grammar)
                                 (declare-language-build-strategy 'rust-rowan
                                   (make-rust-rowan-strategy hcl-language-grammar)))
         fixtures: hcl-official-fixtures
         native-test-profile: (.o profile: 'recursive-source
                                  digest: direct-hcl-grammar-digest
                                  source: direct-parse-hcl lexer: direct-lex-hcl)))
