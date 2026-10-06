;;; -*- Gerbil -*-
;;; Research language only: explicit operand tiers, no numeric precedence.
(import "expression-vocabulary"
        (only-in "../../../src/language/grammar" deflanguage))
(export tier-study-parser)

(deflanguage tier-study
  (identity "tier-study" "v1" "tier-study.local.v1")
  (root source-file)
  (lex
   (whitespace Whitespace (whitespace+))
   (number Number (decimal-digit+))
   (identifier Identifier (identifier))
   (punctuation Punctuation (literals "+" "-" "*" "**" "/" "(" ")"))
   (unknown Unknown (fallback)))
  (rules
   (source-file (node SourceFile (field expression (reference sum))))
   (sum
    (left-binary-tier sum (reference product)
      (choice (literal "+") (literal "-"))
      (node Expression) (fields left operator right)))
   (product
    (left-binary-tier product (reference power)
      (choice (literal "*") (literal "/"))
      (node Expression) (fields left operator right)))
   (power
    (right-binary-tier power (reference prefix) (literal "**")
      (node Expression) (fields left operator right)))
   ;; Explicit study policy: prefix binds more tightly than exponentiation.
   (prefix
    (choice
     (node PrefixExpression
       (seq (field operator (choice (literal "+") (literal "-")))
            (field operand (reference prefix))))
     (reference atom)))
   (atom
    (choice
     (node GroupedExpression
       (seq (literal "(") (field expression (reference sum)) (literal ")")))
     (node NameExpression (field name (token identifier)))
     (node NumberExpression (field value (token number))))))
  (extras whitespace)
  (keywords) (recoveries)
  (conflicts reject) (case-insensitive #f))
