;;; -*- Gerbil -*-
;;; HCL v2.24.0 official specsuite sources embedded at expansion time.

(import :gerbil-parser/languages/hcl/v2-24/parser
        ../../support/fixture)
(export hcl-v2-24-representative-fixture
        hcl-v2-24-official-fixtures
        hcl-v2-24-official-accepted-fixtures
        hcl-v2-24-official-rejected-fixtures)

(defsyntax-fixture hcl-v2-24-representative-fixture
  (identity "hcl/v2.24.0/representative"
            "hcl" +hcl-native-syntax-version+ +hcl-syntax-contract-v1+)
  (source "corpus/representative.hcl")
  (expect accepted HclFile
          (Block Attribute TupleExpression ObjectExpression
                 TraversalExpression)))

(defsyntax-corpus hcl-v2-24-official-fixtures
  (identity "hcl" +hcl-native-syntax-version+ +hcl-syntax-contract-v1+)
  (accepted
   ("hcl/specsuite/comments/hash" hcl-spec-hash-comment
    "corpus/hashicorp-v2.24.0/comments/hash_comment.hcl" HclFile ())
   ("hcl/specsuite/comments/multiline" hcl-spec-multiline-comment
    "corpus/hashicorp-v2.24.0/comments/multiline_comment.hcl" HclFile ())
   ("hcl/specsuite/comments/slash" hcl-spec-slash-comment
    "corpus/hashicorp-v2.24.0/comments/slash_comment.hcl" HclFile ())
   ("hcl/specsuite/empty" hcl-spec-empty
    "corpus/hashicorp-v2.24.0/empty.hcl" HclFile ())
   ("hcl/specsuite/expressions/heredoc" hcl-spec-heredoc
    "corpus/hashicorp-v2.24.0/expressions/heredoc.hcl"
    HclFile (Attribute ObjectExpression HeredocExpression))
   ("hcl/specsuite/expressions/operators" hcl-spec-operators
    "corpus/hashicorp-v2.24.0/expressions/operators.hcl"
    HclFile (Block Attribute BinaryExpression ConditionalExpression))
   ("hcl/specsuite/expressions/primitive-literals" hcl-spec-primitives
    "corpus/hashicorp-v2.24.0/expressions/primitive_literals.hcl"
    HclFile (Attribute NumberExpression StringExpression LiteralExpression))
   ("hcl/specsuite/structure/attributes/expected" hcl-spec-attributes-expected
    "corpus/hashicorp-v2.24.0/structure/attributes_expected.hcl"
    HclFile (Attribute StringExpression))
   ("hcl/specsuite/structure/attributes/unexpected" hcl-spec-attributes-unexpected
    "corpus/hashicorp-v2.24.0/structure/attributes_unexpected.hcl"
    HclFile (Attribute StringExpression))
   ("hcl/specsuite/structure/blocks/empty-oneline" hcl-spec-block-empty-oneline
    "corpus/hashicorp-v2.24.0/structure/block_empty_oneline.hcl"
    HclFile (Block))
   ("hcl/specsuite/structure/blocks/empty-multiline" hcl-spec-block-empty-multiline
    "corpus/hashicorp-v2.24.0/structure/block_empty_multiline.hcl"
    HclFile (Block))
   ("hcl/specsuite/structure/blocks/single-oneline" hcl-spec-block-single-oneline
    "corpus/hashicorp-v2.24.0/structure/block_single_oneline.hcl"
    HclFile (Block Attribute StringExpression)))
  (rejected
   ("hcl/specsuite/structure/attributes/singleline-bad"
    hcl-spec-attribute-singleline-bad
    "corpus/hashicorp-v2.24.0/invalid/attribute_singleline_bad.hcl")
   ("hcl/specsuite/structure/blocks/single-oneline-invalid"
    hcl-spec-block-single-oneline-invalid
    "corpus/hashicorp-v2.24.0/invalid/block_single_oneline_invalid.hcl")
   ("hcl/specsuite/structure/blocks/single-unclosed"
    hcl-spec-block-single-unclosed
    "corpus/hashicorp-v2.24.0/invalid/block_single_unclosed.hcl")))

(def hcl-v2-24-official-accepted-fixtures
  (filter (lambda (fixture)
            (eq? (syntax-fixture-expected-status fixture) 'accepted))
          hcl-v2-24-official-fixtures))

(def hcl-v2-24-official-rejected-fixtures
  (filter (lambda (fixture)
            (eq? (syntax-fixture-expected-status fixture) 'rejected))
          hcl-v2-24-official-fixtures))
