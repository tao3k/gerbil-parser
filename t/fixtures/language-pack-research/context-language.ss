;;; -*- Gerbil -*-
;;; Research language only: explicit operand tiers, no numeric precedence.
(import (rename-in "context-vocabulary" (deflanguage/tiers define-contextual-language)))
(export contextual-study-parser context-host-values)

;;; Host values deliberately share grammar-rule spellings.
(def sum 'host-sum)
(def product 'host-product)
(def power 'host-power)
(def context-host-values (list sum product power))

(define-contextual-language contextual-study
  (identity "contextual-study" "v1" "contextual-study.local.v1")
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
    (left-tier (reference product)
      (choice (literal "+") (literal "-"))
      (node Expression) (fields left operator right)))
   (product
    (left-tier (reference power)
      (choice (literal "*") (literal "/"))
      (node Expression) (fields left operator right)))
   (power
    (right-tier (reference prefix) (literal "**")
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
