;;; -*- Gerbil -*-
;;; Canonical left-recursive arithmetic grammar for LR precedence admission.

(import :gerbil-parser/src/language/grammar)
(export +arithmetic-language-version+
        +arithmetic-syntax-contract+
        arithmetic-language-grammar
        arithmetic-grammar
        arithmetic-parser-ir
        arithmetic-parser)

(def +arithmetic-language-version+ "v1")
(def +arithmetic-syntax-contract+ "arithmetic-expression.v1")

(deflanguage arithmetic
  (identity "arithmetic" +arithmetic-language-version+
            +arithmetic-syntax-contract+)
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
  (case-insensitive #f))
