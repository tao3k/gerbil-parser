;;; -*- Gerbil -*-
;;; Canonical left-recursive arithmetic grammar for LR precedence admission.

(import (only-in :gerbil-parser/language-support/grammar deflanguage)
        (only-in :gerbil-parser/src/compiler/arithmetic-drive direct-drive direct-grammar-digest))
(export arithmetic-syntax
        arithmetic-grammar
        arithmetic-parser-ir
        arithmetic-parser)


(deflanguage arithmetic
  (root source-file)
  (lex
   (whitespace Whitespace (whitespace+))
   (number Number (decimal-digit+))
   (identifier Identifier (identifier))
   (punctuation Punctuation (literals "+" "-" "*" "**" "/" "(" ")"))
   (unknown Unknown (fallback)))
  (rules
   (source-file
    (node SourceFile (field expression expression)))
   (expression
    (choice
     (prec left 10
      (node Expression
       (seq
        (field left expression)
        (field operator (choice (literal "+") (literal "-")))
        (field right expression))))
     (prec left 20
      (node Expression
       (seq
        (field left expression)
        (field operator (choice (literal "*") (literal "/")))
        (field right expression))))
     (prec right 30 prefix-expression)
     grouped-expression
     name-expression
     number-expression))
   (grouped-expression
    (node GroupedExpression
      (seq
       (literal "(")
       (field expression expression)
       (literal ")"))))
   (prefix-expression
    (prec right 30
     (node PrefixExpression
       (seq
        (field operator (choice (literal "+") (literal "-")))
        (field operand expression)))))
   (name-expression
    (node NameExpression (field name identifier)))
   (number-expression
    (node NumberExpression (field value number))))
  (extras whitespace)
  (keywords)
  (recoveries
   (expression "GERBIL-PARSER-EXPRESSION" preserve-source))
  (conflicts reject)
  (case-insensitive #f)
  (backends (drive direct-grammar-digest direct-drive)))
