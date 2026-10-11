#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import "expression-vocabulary"
        (only-in "../../../src/grammar/algebra" grammar-expression))

(def (comparison-check label actual expected)
  (unless (equal? actual expected)
    (error "expression vocabulary mismatch" label actual expected))
  (display "EXPRESSION-CASE-OK: ") (display label) (newline) (force-output))

(def arithmetic-binary
  (vocabulary-value
   (binary-expression left 10 (reference expression)
     (choice (literal "+") (literal "-"))
     (node Expression) (fields left operator right))))
(comparison-check 'arithmetic-core arithmetic-binary
  (grammar-expression
   (prec left 10
     (alias Expression
       (seq (field left (reference expression))
            (field operator (choice (literal "+") (literal "-")))
            (field right (reference expression)))))))

(def hcl-chain
  (vocabulary-value
   (expression-chain (reference unary-expression) (reference binary-operator)
     (node BinaryExpression) (fields left operator right))))
(comparison-check 'hcl-core hcl-chain
  (grammar-expression
   (alias BinaryExpression
     (seq (field left (reference unary-expression))
          (repeat
           (seq (field operator (reference binary-operator))
                (field right (reference unary-expression))))))))

(comparison-check 'binary-defaults
  (vocabulary-value
   (binary-expression left 10 (reference expression) (literal "+")))
  (vocabulary-value
   (binary-expression left 10 (reference expression) (literal "+")
     (node BinaryExpression) (fields left operator right))))
(comparison-check 'chain-defaults
  (vocabulary-value
   (expression-chain (reference unary-expression) (reference binary-operator)))
  hcl-chain)
(comparison-check 'explicit-fields
  (vocabulary-value
   (expression-chain (reference term) (literal "+")
     (node Sum) (fields first operation rest)))
  (grammar-expression
   (alias Sum
     (seq (field first (reference term))
          (repeat (seq (field operation (literal "+"))
                       (field rest (reference term))))))))
(comparison-check 'binary-and-chain-distinct (equal? arithmetic-binary hcl-chain) #f)
(display "EXPRESSION-COMPARISON-OK: 6 checks passed") (newline) (force-output)
