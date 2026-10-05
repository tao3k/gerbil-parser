;;; -*- Gerbil -*-
;;; Version-pinned HCL native syntax grammar owner.

(import (only-in :gerbil-parser/language-support deflanguage)
        (only-in :gerbil-parser/src/compiler/hcl-reductions direct-step direct-event-step direct-grammar-digest)
        (only-in :gerbil-parser/src/compiler/hcl-source
                 direct-parse-hcl direct-hcl-grammar-digest))
(export +hcl-native-syntax-version+
        +hcl-native-syntax-commit+
        +hcl-syntax-contract+
        hcl-language-grammar
        hcl-grammar
        hcl-parser-ir
        hcl-parser)

(def +hcl-native-syntax-version+ "v2.24.0")
(def +hcl-native-syntax-commit+
  "6b5068090eef06b1f127f61529db5ba0be7ed343")
(def +hcl-syntax-contract+ "hcl-native-v2.24.0.v1")

(deflanguage hcl
  (identity "hcl" +hcl-native-syntax-version+ +hcl-syntax-contract+)
  (root config-file)
  (lex
   (horizontal-whitespace HorizontalWhitespace (horizontal-whitespace+))
   (newline Newline (newline+))
   (comment Comment (line-comment "#" "//"))
   (block-comment-token Comment (block-comment "/*" "*/"))
   (heredoc Heredoc (heredoc))
   (string String (quoted-string "\""))
   (number Number (number))
   (identifier Identifier (identifier))
   (punctuation Punctuation
    (literals "==" "!=" "<=" ">=" "&&" "||"
              "{" "}" "[" "]" "(" ")" "=" "," "." ":"
              "+" "-" "*" "/" "%" "!" "<" ">" "?"))
   (unknown Unknown (fallback)))
  (rules
   (config-file
    (alias HclFile
      (repeat (choice (token newline) (field item (reference body-item))))))
   (body
    (alias Body
      (repeat (choice (token newline) (field item (reference body-item))))))
   (body-item
    (choice (reference block) (reference attribute)))
   (attribute
    (alias Attribute
      (seq
       (field name (token identifier))
       (literal "=")
       (field value (reference expression)))))
   (block
    (alias Block
      (seq
       (field type (token identifier))
       (repeat
        (field label (choice (token string) (token identifier))))
       (literal "{")
       (field body (reference body))
       (literal "}"))))
   (expression (reference conditional-expression))
   (conditional-expression
    (alias ConditionalExpression
      (seq
       (field condition (reference binary-expression))
       (optional
        (seq
         (literal "?")
         (field consequent (reference expression))
         (literal ":")
         (field alternative (reference expression)))))))
   (binary-expression
    (alias BinaryExpression
      (seq
       (field left (reference unary-expression))
       (repeat
        (seq
         (field operator (reference binary-operator))
         (field right (reference unary-expression)))))))
   (binary-operator
    (choice
     (literal "==") (literal "!=") (literal "<=") (literal ">=")
     (literal "&&") (literal "||") (literal "+") (literal "-")
     (literal "*") (literal "/") (literal "%") (literal "<")
     (literal ">")))
   (unary-expression
    (alias UnaryExpression
      (seq
       (repeat (field operator
                       (choice (literal "!") (literal "-") (literal "+"))))
       (field operand (reference postfix-expression)))))
   (postfix-expression
    (alias TraversalExpression
      (seq
       (field root (reference primary-expression))
       (repeat (field step (reference postfix-suffix))))))
   (postfix-suffix
    (choice
     (seq (literal ".")
          (choice (token identifier) (token number) (literal "*")))
     (alias IndexExpression
       (seq (literal "[")
            (field key (choice (literal "*") (reference expression)))
            (literal "]")))
     (alias CallExpression
       (seq
        (literal "(")
        (optional
         (seq
          (field argument (reference expression))
          (repeat
           (seq (literal ",") (field argument (reference expression))))))
        (literal ")")))))
   (primary-expression
    (choice
     (reference object-expression)
     (reference tuple-expression)
     (reference heredoc-expression)
     (reference string-expression)
     (reference number-expression)
     (alias LiteralExpression (field value (token identifier)))
     (seq (literal "(") (reference expression) (literal ")"))))
   (traversal-expression
    (alias TraversalExpression
      (seq
       (field root (token identifier))
       (repeat
        (seq (literal ".") (field step (token identifier)))))))
   (string-expression
    (alias StringExpression (field value (token string))))
   (heredoc-expression
    (alias HeredocExpression (field value (token heredoc))))
   (number-expression
    (alias NumberExpression (field value (token number))))
   (tuple-expression
    (alias TupleExpression
      (seq
       (literal "[")
       (repeat (token newline))
       (optional (reference tuple-elements))
       (literal "]"))))
   (tuple-elements
    (seq
     (field element (reference expression))
     (optional (reference tuple-tail))))
   (tuple-tail
    (choice
     (seq (literal ",")
          (repeat (token newline))
          (optional (reference tuple-elements)))
     (seq (repeat1 (token newline))
          (optional (reference tuple-elements)))))
   (object-expression
    (alias ObjectExpression
      (seq
       (literal "{")
       (repeat
        (choice
         (token newline)
         (field entry (reference object-entry))))
       (literal "}"))))
   (object-entry
    (alias ObjectEntry
      (seq
       (field key (choice (token identifier) (token string)))
       (choice (literal "=") (literal ":"))
       (field value (reference expression))
       (optional (literal ","))))))
  (extras horizontal-whitespace comment block-comment-token)
  (keywords)
  (recoveries
   (config-file "GERBIL-PARSER-HCL-V2-24" preserve-source))
  (conflicts reject)
  (case-insensitive #f)
  (node-fields
   (CallExpression callee argument)
   (IndexExpression collection key))
  (flow
   (source lexical)
   (lexical hcl-structural-core)
   (hcl-structural-core cst))
  (backends
   (step direct-grammar-digest direct-step)
   (source direct-hcl-grammar-digest direct-parse-hcl)
   (event-step direct-grammar-digest direct-event-step)))

;;; Corpus declarations share the grammar admission boundary.
(import (only-in :gerbil-parser/language-support
                 defsyntax-corpus defsyntax-fixture syntax-fixture-expected-status))
(export hcl-representative-fixture hcl-official-fixtures hcl-official-accepted-fixtures hcl-official-rejected-fixtures)

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

(def hcl-official-accepted-fixtures
  (filter (lambda (fixture)
            (eq? (syntax-fixture-expected-status fixture) 'accepted))
          hcl-official-fixtures))

(def hcl-official-rejected-fixtures
  (filter (lambda (fixture)
            (eq? (syntax-fixture-expected-status fixture) 'rejected))
          hcl-official-fixtures))
