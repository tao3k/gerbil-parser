;;; -*- Gerbil -*-
(import :gerbil-parser/language-test-support
        (only-in :gerbil-parser/src/compiler/machine parser-machine-direct-drive)
        ./parser)
(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-support/fixture defsyntax-fixture)
        (only-in :gerbil-parser/language-support/development deflanguage-development-loader LanguageDevelopmentLoader.)
        (only-in :gerbil-parser/language-build-support declare-language-build-strategy make-fused-reduction-strategy make-rust-runtime-strategy))
(export arithmetic-basic-fixture arithmetic-test-language)

(defsyntax-fixture arithmetic-basic-fixture
  (identity "arithmetic/v1/basic"
            "arithmetic"
            "v1"
            "arithmetic-expression.v1")
  (source "corpus/basic.expr")
  (expect accepted SourceFile (Expression)))
(deflanguage-development-loader arithmetic-test-language
  (grammar arithmetic-language-grammar)
  (parse parse-arithmetic-test)
  (slots metadata: '((grammar-format . concise-dsl))
         build-strategies: (list (declare-language-build-strategy 'fused-reductions
                                   (make-fused-reduction-strategy arithmetic-language-grammar))
                                 (declare-language-build-strategy 'rust-runtime
                                   (make-rust-runtime-strategy arithmetic-language-grammar)))
         fixtures: (list arithmetic-basic-fixture)))

(deflanguage-parser-tests arithmetic-parser-test "arithmetic v1 language pack"
  (loader arithmetic-test-language)
  (property "compiled machine installs its direct LR driver"
    (bindings)
    (equal (procedure? (parser-machine-direct-drive arithmetic-parser)) #t))
  (identity "declarative entry identity"
    (language "arithmetic") (version "v1") (contract "arithmetic-expression.v1")
    (schema "gerbil-parser.language-entry.v2"))
  (fixtures "colocated native fixtures")
  (accepted "streaming LR preserves the 1024-line source"
    (string-join (make-list 1024 "1") "+\n"))
  (ast "canonical AST shape is exact" "1"
    (node SourceFile 0 1
      (field expression 0 1
        (node NumberExpression 0 1
          (field value 0 1 (token number "1" 0 1)))))))
