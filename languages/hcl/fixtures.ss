;;; -*- Gerbil -*-
;;; Declarative pinned corpus and generated native-route qualification.
(import  ./parser)
(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-support/fixture defsyntax-corpus defsyntax-fixture)
        (only-in :gerbil-parser/language-support/development deflanguage-development-loader LanguageDevelopmentLoader.)
        (only-in :gerbil-parser/language-build-support declare-language-build-strategy make-fused-reduction-strategy make-rust-runtime-strategy)
        (only-in :gerbil-parser/src/compiler/hcl-source direct-parse-hcl direct-lex-hcl direct-hcl-grammar-digest))
(export #t)

(defsyntax-fixture hcl-representative-fixture
  (identity "hcl/v2.24.0/representative"
            "hcl" +hcl-native-syntax-version+ +hcl-syntax-contract+)
  (source "corpus/representative.hcl")
  (expect accepted HclFile
          (Block Attribute TupleExpression ObjectExpression
                 TraversalExpression)))

(defsyntax-corpus hcl-official-fixtures
  (identity "hcl" +hcl-native-syntax-version+ +hcl-syntax-contract+)
  (accepted
   ("hcl/specsuite/comments/hash" hcl-spec-hash-comment
    "corpus/reference/comments/hash_comment.hcl" HclFile ())
   ("hcl/specsuite/comments/multiline" hcl-spec-multiline-comment
    "corpus/reference/comments/multiline_comment.hcl" HclFile ())
   ("hcl/specsuite/comments/slash" hcl-spec-slash-comment
    "corpus/reference/comments/slash_comment.hcl" HclFile ())
   ("hcl/specsuite/empty" hcl-spec-empty
    "corpus/reference/empty.hcl" HclFile ())
   ("hcl/specsuite/expressions/heredoc" hcl-spec-heredoc
    "corpus/reference/expressions/heredoc.hcl"
    HclFile (Attribute ObjectExpression HeredocExpression))
   ("hcl/specsuite/expressions/operators" hcl-spec-operators
    "corpus/reference/expressions/operators.hcl"
    HclFile (Block Attribute BinaryExpression ConditionalExpression))
   ("hcl/specsuite/expressions/primitive-literals" hcl-spec-primitives
    "corpus/reference/expressions/primitive_literals.hcl"
    HclFile (Attribute NumberExpression StringExpression LiteralExpression))
   ("hcl/specsuite/structure/attributes/expected" hcl-spec-attributes-expected
    "corpus/reference/structure/attributes_expected.hcl"
    HclFile (Attribute StringExpression))
   ("hcl/specsuite/structure/attributes/unexpected" hcl-spec-attributes-unexpected
    "corpus/reference/structure/attributes_unexpected.hcl"
    HclFile (Attribute StringExpression))
   ("hcl/specsuite/structure/blocks/empty-oneline" hcl-spec-block-empty-oneline
    "corpus/reference/structure/block_empty_oneline.hcl"
    HclFile (Block))
   ("hcl/specsuite/structure/blocks/empty-multiline" hcl-spec-block-empty-multiline
    "corpus/reference/structure/block_empty_multiline.hcl"
    HclFile (Block))
   ("hcl/specsuite/structure/blocks/single-oneline" hcl-spec-block-single-oneline
    "corpus/reference/structure/block_single_oneline.hcl"
    HclFile (Block Attribute StringExpression)))
  (rejected
   ("hcl/specsuite/structure/attributes/singleline-bad"
    hcl-spec-attribute-singleline-bad
    "corpus/reference/invalid/attribute_singleline_bad.hcl")
   ("hcl/specsuite/structure/blocks/single-oneline-invalid"
    hcl-spec-block-single-oneline-invalid
    "corpus/reference/invalid/block_single_oneline_invalid.hcl")
   ("hcl/specsuite/structure/blocks/single-unclosed"
    hcl-spec-block-single-unclosed
    "corpus/reference/invalid/block_single_unclosed.hcl")))
(deflanguage-development-loader hcl-test-language
  (grammar hcl-language-grammar)
  (parse parse-hcl-test)
  (slots metadata: `((grammar-format . concise-dsl) (reference-commit . ,+hcl-native-syntax-commit+))
         build-strategies: (list (declare-language-build-strategy 'fused-reductions
                                   (make-fused-reduction-strategy hcl-language-grammar))
                                 (declare-language-build-strategy 'rust-runtime
                                   (make-rust-runtime-strategy hcl-language-grammar)))
         fixtures: hcl-official-fixtures
         native-test-profile: (.o profile: 'recursive-source
                                  digest: direct-hcl-grammar-digest
                                  source: direct-parse-hcl lexer: direct-lex-hcl)))

