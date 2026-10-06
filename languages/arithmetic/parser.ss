;;; -*- Gerbil -*-
;;; Canonical public parser entry for the arithmetic v1 reference language.

(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-build-support
                 declare-language-build-strategy make-fused-reduction-strategy make-rust-rowan-strategy)
        (only-in :gerbil-parser/language-support/development deflanguage-development-loader LanguageDevelopmentLoader.)
        (only-in ./grammar
                 +arithmetic-language-version+
                 +arithmetic-syntax-contract+
                 arithmetic-language-grammar
                 arithmetic-basic-fixture
                 arithmetic-grammar
                 arithmetic-parser-ir
                 arithmetic-parser))
(export arithmetic-basic-fixture
        +arithmetic-language-version+
        +arithmetic-syntax-contract+
        arithmetic-language-grammar
        arithmetic-grammar
        arithmetic-parser-ir
        arithmetic-parser
        arithmetic-language
        parse-arithmetic)

(deflanguage-development-loader (arithmetic-language :: self LanguageDevelopmentLoader.)
  (grammar arithmetic-language-grammar)
  (parse parse-arithmetic)
  (slots metadata: (.o grammar-format: 'concise-dsl)
         build-strategies: (list (declare-language-build-strategy 'fused-reductions
                                   (make-fused-reduction-strategy arithmetic-language-grammar))
                                 (declare-language-build-strategy 'rust-rowan
                                   (make-rust-rowan-strategy arithmetic-language-grammar)))
         fixtures: (list arithmetic-basic-fixture)))
