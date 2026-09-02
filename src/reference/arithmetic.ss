;;; Executable reference grammar proving object-family composition and generation.

(import ../modules/parser/config)
(export arithmetic-grammar
        arithmetic-parser-ir
        arithmetic-parser)

(defparser-config arithmetic-lexical-role
  arithmetic-grammar
  arithmetic-parser-ir
  arithmetic-parser
  (syntax-kinds
   (SourceFile node (expression))
   (PrefixExpression node (operator operand))
   (BinaryExpression node (left operator right))
   (NameExpression node (name))
   (NumberExpression node (value))
   (Identifier token (text))
   (Number token (text))
   (Whitespace token (text))
   (Punctuation token (text))
   (Unknown token (text)))
  (terminals
   (identifier Identifier)
   (number Number)
   (whitespace Whitespace)
   (punctuation Punctuation))
  (rules
   (source-file
    (alias SourceFile
      (field expression (reference expression))))
   (expression
    (choice
     (reference prefix-expression)
     (reference binary-expression)
     (reference name-expression)
     (reference number-expression)
     (seq (literal "(")
          (reference expression)
          (literal ")"))))
   (prefix-expression
    (alias PrefixExpression
      (seq
       (field operator
         (choice (literal "+") (literal "-")))
       (field operand (reference expression)))))
   (binary-expression
    (alias BinaryExpression
      (seq
       (field left (reference expression))
       (field operator
         (choice
          (literal "+")
          (literal "-")
          (literal "*")
          (literal "/")))
       (field right (reference expression)))))
   (name-expression
    (alias NameExpression
      (field name (token identifier))))
   (number-expression
    (alias NumberExpression
      (field value (token number)))))
  (extras whitespace)
  (keywords)
  (prefix-operators
   ((punctuation "+") 30 right)
   ((punctuation "-") 30 right))
  (binary-operators
   ((punctuation "+") 10 left)
   ((punctuation "-") 10 left)
   ((punctuation "*") 20 left)
   ((punctuation "/") 20 left))
  (parser-entrypoints
   (source-file parse pure))
  (recoveries
   (expression "GERBIL-PARSER-EXPRESSION" preserve-source))
  (flow
   (source lexical)
   (lexical expression)
   (expression cst)))
