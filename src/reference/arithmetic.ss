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
  (keywords)
  (prefix-operators
   ((punctuation "+") 30 right)
   ((punctuation "-") 30 right))
  (binary-operators
   ((punctuation "+") 10 left)
   ((punctuation "-") 10 left)
   ((punctuation "*") 20 left)
   ((punctuation "/") 20 left))
  (parser-entrypoints)
  (recoveries
   (expression "GERBIL-PARSER-EXPRESSION" preserve-source))
  (flow
   (source lexical)
   (lexical expression)
   (expression cst)))
