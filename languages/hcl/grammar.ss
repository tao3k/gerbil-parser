;;; -*- Gerbil -*-
;;; Version-pinned HCL native syntax grammar owner.

(import (only-in :gerbil-parser/language-support/grammar deflanguage))
(export +hcl-native-syntax-commit+
        hcl-syntax
        hcl-grammar
        hcl-parser-ir
        hcl-parser)

(def +hcl-native-syntax-commit+
     "6b5068090eef06b1f127f61529db5ba0be7ed343")

(deflanguage hcl
  (syntax
    (lexical
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
     (extras horizontal-whitespace comment block-comment-token)
     (keywords)
     (recoveries
      (config-file "GERBIL-PARSER-HCL-V2-24" preserve-source))
     (conflicts reject)
     (case-insensitive #f)
     (node-fields
      (CallExpression callee argument)
      (IndexExpression collection key))))
  (rules
    (config-file
     (node HclFile
           (repeat (choice (token newline) (field item (reference body-item))))))
    (body
     (node Body
           (repeat (choice (token newline) (field item (reference body-item))))))
    (body-item
     (choice (reference block) (reference attribute)))
    (attribute
     (node Attribute
           (seq
            (field name (token identifier))
            (literal "=")
            (field value (reference expression)))))
    (block
     (node Block
           (seq
            (field type (token identifier))
            (repeat
             (field label (choice (token string) (token identifier))))
            (literal "{")
            (field body (reference body))
            (literal "}"))))
    (expression (reference conditional-expression))
    (conditional-expression
     (node ConditionalExpression
           (seq
            (field condition (reference binary-expression))
            (optional
             (seq
              (literal "?")
              (field consequent (reference expression))
              (literal ":")
              (field alternative (reference expression)))))))
    (binary-expression
     (node BinaryExpression
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
     (node UnaryExpression
           (seq
            (repeat (field operator
                           (choice (literal "!") (literal "-") (literal "+"))))
            (field operand (reference postfix-expression)))))
    (postfix-expression
     (node TraversalExpression
           (seq
            (field root (reference primary-expression))
            (repeat (field step (reference postfix-suffix))))))
    (postfix-suffix
     (choice
      (seq (literal ".")
           (choice (token identifier) (token number) (literal "*")))
      (node IndexExpression
            (seq (literal "[")
                 (field key (choice (literal "*") (reference expression)))
                 (literal "]")))
      (node CallExpression
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
      (node LiteralExpression (field value (token identifier)))
      (seq (literal "(") (reference expression) (literal ")"))))
    (traversal-expression
     (node TraversalExpression
           (seq
            (field root (token identifier))
            (repeat
             (seq (literal ".") (field step (token identifier)))))))
    (string-expression
     (node StringExpression (field value (token string))))
    (heredoc-expression
     (node HeredocExpression (field value (token heredoc))))
    (number-expression
     (node NumberExpression (field value (token number))))
    (tuple-expression
     (node TupleExpression
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
     (node ObjectExpression
           (seq
            (literal "{")
            (repeat
             (choice
              (token newline)
              (field entry (reference object-entry))))
            (literal "}"))))
    (object-entry
     (node ObjectEntry
           (seq
            (field key (choice (token identifier) (token string)))
            (choice (literal "=") (literal ":"))
            (field value (reference expression))
            (optional (literal ",")))))))
