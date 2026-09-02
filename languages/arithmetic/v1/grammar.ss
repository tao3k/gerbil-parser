;;; -*- Gerbil -*-
;;; Small generic-grammar fixture for precedence expressed as grammar levels.

(import :gerbil-parser/src/modules/parser/config)
(export arithmetic-grammar
        arithmetic-parser-ir
        arithmetic-parser)

(defparser-config arithmetic-lexical-role
  arithmetic-grammar
  arithmetic-parser-ir
  arithmetic-parser
  (syntax-kinds
   (SourceFile node (expression))
   (Expression node (left operator right))
   (Term node (left operator right))
   (GroupedExpression node (expression))
   (PrefixExpression node (operator operand))
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
   (punctuation Punctuation)
   (unknown Unknown))
  (lexical-rules
   (whitespace (whitespace+))
   (number (decimal-digit+))
   (identifier (identifier))
   (punctuation (literals "+" "-" "*" "**" "/" "(" ")"))
   (unknown (fallback)))
  (rules
   (source-file
    (alias SourceFile
      (field expression (reference expression))))
   (expression
    (alias Expression
      (seq
       (field left (reference term))
       (repeat
        (seq
         (field operator (choice (literal "+") (literal "-")))
         (field right (reference term)))))))
   (term
    (alias Term
      (seq
       (field left (reference primary))
       (repeat
        (seq
         (field operator (choice (literal "*") (literal "/")))
         (field right (reference primary)))))))
   (primary
    (choice
     (reference prefix-expression)
     (reference grouped-expression)
     (reference name-expression)
     (reference number-expression)))
   (grouped-expression
    (alias GroupedExpression
      (seq
       (literal "(")
       (field expression (reference expression))
       (literal ")"))))
   (prefix-expression
    (alias PrefixExpression
      (seq
       (field operator (choice (literal "+") (literal "-")))
       (field operand (reference primary)))))
   (name-expression
    (alias NameExpression
      (field name (token identifier))))
   (number-expression
    (alias NumberExpression
      (field value (token number)))))
  (extras whitespace)
  (keywords)
  (parser-entrypoints
   (source-file parse pure))
  (recoveries
   (expression "GERBIL-PARSER-EXPRESSION" preserve-source))
  (flow
   (source lexical)
   (lexical expression)
   (expression cst)))
