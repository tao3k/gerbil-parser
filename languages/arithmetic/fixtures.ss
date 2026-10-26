;;; -*- Gerbil -*-
(import 
        (only-in :gerbil-parser/src/compiler/machine parser-machine-direct-drive)
        ./parser)
(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-support/fixture defsyntax-fixture)
        (only-in :gerbil-parser/language-support/development deflanguage-development-loader LanguageDevelopmentLoader.)
        (only-in :gerbil-parser/language-build-support declare-language-build-strategy make-fused-reduction-strategy make-rust-runtime-strategy))
(export #t)

(defsyntax-fixture arithmetic-basic-fixture
  (identity "arithmetic/v1/basic"
            "arithmetic"
            "v1"
            "arithmetic-expression.v1")
  (source "corpus/basic.expr")
  (expect accepted SourceFile (Expression)))
(defsyntax-fixture arithmetic-incomplete-fixture
  (identity "arithmetic/v1/incomplete" "arithmetic" "v1" "arithmetic-expression.v1")
  (text "1 +")
  (expect rejected #f ()))
(deflanguage-development-loader arithmetic-test-language
  (grammar arithmetic-language-grammar)
  (parse parse-arithmetic-test)
  (slots metadata: '((grammar-format . concise-dsl))
         build-strategies: (list (declare-language-build-strategy 'fused-reductions
                                   (make-fused-reduction-strategy arithmetic-language-grammar))
                                 (declare-language-build-strategy 'rust-runtime
                                   (make-rust-runtime-strategy arithmetic-language-grammar)))
         fixtures: (list arithmetic-basic-fixture arithmetic-incomplete-fixture)))

