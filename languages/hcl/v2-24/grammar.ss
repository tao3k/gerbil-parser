;;; -*- Gerbil -*-
;;; Version-pinned HCL native syntax grammar owner.

(import :gerbil-parser/src/modules/parser/config)
(export +hcl-native-syntax-version+
        +hcl-native-syntax-commit+
        +hcl-syntax-contract-v1+
        hcl-v2-24-grammar
        hcl-v2-24-parser-ir
        hcl-v2-24-parser-machine)

(def +hcl-native-syntax-version+ "v2.24.0")
(def +hcl-native-syntax-commit+
  "6b5068090eef06b1f127f61529db5ba0be7ed343")
(def +hcl-syntax-contract-v1+ "hcl-native-v2.24.0.v1")

(defparser-config hcl-v2-24-role
  hcl-v2-24-grammar
  hcl-v2-24-parser-ir
  hcl-v2-24-parser-machine
  (syntax-kinds
   (HclFile node (item))
   (Body node (item))
   (Attribute node (name value))
   (Block node (type label body))
   (TraversalExpression node (root step))
   (CallExpression node (callee argument))
   (BinaryExpression node (left operator right))
   (UnaryExpression node (operator operand))
   (ConditionalExpression node (condition consequent alternative))
   (IndexExpression node (collection key))
   (StringExpression node (value))
   (HeredocExpression node (value))
   (NumberExpression node (value))
   (LiteralExpression node (value))
   (TupleExpression node (element))
   (ObjectExpression node (entry))
   (ObjectEntry node (key value))
   (Identifier token (text))
   (Number token (text))
   (String token (text))
   (Heredoc token (text))
   (HorizontalWhitespace token (text))
   (Newline token (text))
   (Comment token (text))
   (Punctuation token (text))
   (Unknown token (text)))
  (terminals
   (identifier Identifier)
   (number Number)
   (string String)
   (heredoc Heredoc)
   (horizontal-whitespace HorizontalWhitespace)
   (newline Newline)
   (comment Comment)
   (block-comment-token Comment)
   (punctuation Punctuation)
   (unknown Unknown))
  (lexical-rules
   (horizontal-whitespace (horizontal-whitespace+))
   (newline (newline+))
   (comment (line-comment "#" "//"))
   (block-comment-token (block-comment "/*" "*/"))
   (heredoc (heredoc))
   (string (quoted-string "\""))
   (number (number))
   (identifier (identifier))
   (punctuation
    (literals "==" "!=" "<=" ">=" "&&" "||"
              "{" "}" "[" "]" "(" ")" "=" "," "." ":"
              "+" "-" "*" "/" "%" "!" "<" ">" "?"))
   (unknown (fallback)))
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
       (field value (reference expression))
       (optional (token newline)))))
   (block
    (alias Block
      (seq
       (field type (token identifier))
       (repeat
        (field label (choice (token string) (token identifier))))
       (literal "{")
       (optional (token newline))
       (field body (reference body))
       (literal "}")
       (optional (token newline)))))
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
       (optional
        (seq
         (field element (reference expression))
         (repeat
          (seq
           (choice (literal ",") (token newline))
           (field element (reference expression))))
         (optional (literal ","))))
       (repeat (token newline))
       (literal "]"))))
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
       (optional (literal ","))
       (optional (token newline))))))
  (extras horizontal-whitespace comment block-comment-token)
  (keywords)
  (parser-entrypoints
   (config-file parse pure))
  (recoveries
   (config-file "GERBIL-PARSER-HCL-V2-24" preserve-source))
  (flow
   (source lexical)
   (lexical hcl-structural-core)
   (hcl-structural-core cst)))
