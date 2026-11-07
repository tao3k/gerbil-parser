#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Numeric-free templates versus explicit core grammar structures.
(import "expression-vocabulary"
        (only-in "../../../src/grammar/algebra" grammar-expression))

(def (tier-check label actual expected)
  (unless (equal? actual expected)
    (error "tier vocabulary mismatch" label actual expected))
  (display "TIER-CASE-OK: ") (display label) (newline) (force-output))

(def sum-core
  (vocabulary-value
   (left-binary-tier sum (reference product)
     (choice (literal "+") (literal "-"))
     (node Expression) (fields left operator right))))
(tier-check 'left-binary-structure sum-core
  (grammar-expression
   (choice
    (alias Expression
      (seq (field left (reference sum))
           (field operator (choice (literal "+") (literal "-")))
           (field right (reference product))))
    (reference product))))

(def power-core
  (vocabulary-value
   (right-binary-tier power (reference atom) (literal "**"))))
(tier-check 'right-binary-structure power-core
  (grammar-expression
   (choice
    (alias BinaryExpression
      (seq (field left (reference atom))
           (field operator (literal "**"))
           (field right (reference power))))
    (reference atom))))

(tier-check 'fhirpath-or-tier
  (vocabulary-value
   (expression-chain (reference and-expression)
     (choice (literal "or") (literal "xor"))
     (node Expression) (fields left operator right)))
  (grammar-expression
   (alias Expression
     (seq (field left (reference and-expression))
          (repeat
           (seq (field operator (choice (literal "or") (literal "xor")))
                (field right (reference and-expression))))))))
(tier-check 'fhirpath-implies-tier
  (vocabulary-value
   (recursive-right-chain implies-expression (reference or-expression)
     (literal "implies") (node Expression) (fields left operator right)))
  (grammar-expression
   (alias Expression
     (seq (field left (reference or-expression))
          (optional
           (seq (field operator (literal "implies"))
                (field right (reference implies-expression))))))))

(tier-check 'explicit-node-and-fields
  (vocabulary-value
   (left-binary-tier sum (reference term) (literal "+")
     (node Sum) (fields first operation rest)))
  (grammar-expression
   (choice
    (alias Sum
      (seq (field first (reference sum))
           (field operation (literal "+"))
           (field rest (reference term))))
    (reference term))))

;;; Inspect the admitted values rather than evaluating an alternate parser.
(def (contains-precedence? value)
  (and (pair? value)
       (or (eq? (car value) 'precedence)
           (contains-precedence? (car value))
           (contains-precedence? (cdr value)))))
(tier-check 'numeric-precedence-absent
  (or (contains-precedence? sum-core) (contains-precedence? power-core)) #f)
(display "TIER-COMPARISON-OK: 6 checks passed") (newline) (force-output)
