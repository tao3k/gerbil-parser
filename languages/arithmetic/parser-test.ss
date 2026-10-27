;;; Suites consume the shared corpus/loader Fixture declarations.
(import :gerbil-parser/language-test-support
        (only-in :gerbil-parser/src/runtime/machine parser-machine-direct-drive)
        ./parser)
(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-support/fixture defsyntax-fixture)
        (only-in :gerbil-parser/language-support/development deflanguage-development-loader LanguageDevelopmentLoader.)
        (only-in :gerbil-parser/language-build-support declare-language-build-strategy make-fused-reduction-strategy make-rust-runtime-strategy))
(export arithmetic-basic-fixture arithmetic-incomplete-fixture arithmetic-test-language)
(import ./fixtures)

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
