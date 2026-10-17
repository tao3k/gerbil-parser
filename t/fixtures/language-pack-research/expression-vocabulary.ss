;;; -*- Gerbil -*-
;;; Local design prototype: templates lower to the existing GrammarExpr algebra.
(import (only-in "../../../src/grammar/algebra" grammar-expression)
        (for-syntax (only-in :gerbil/expander core-expand1)))
(export binary-expression expression-chain
        left-binary-tier right-binary-tier recursive-right-chain vocabulary-value)

(defrules binary-expression (node fields)
  ((_ direction rank operand operators)
   (binary-expression direction rank operand operators
     (node BinaryExpression) (fields left operator right)))
  ((_ direction rank operand operators
      (node kind) (fields left-label operator-label right-label))
   (prec direction rank
     (alias kind
       (seq (field left-label operand)
            (field operator-label operators)
            (field right-label operand))))))

(defrules expression-chain (node fields)
  ((_ operand operators)
   (expression-chain operand operators
     (node BinaryExpression) (fields left operator right)))
  ((_ operand operators
      (node kind) (fields left-label operator-label right-label))
   (alias kind
     (seq (field left-label operand)
          (repeat
           (seq (field operator-label operators)
                (field right-label operand)))))))

;;; Numeric-free tier alternatives. The base operand passes through unchanged.
(defrules left-binary-tier (node fields)
  ((_ self operand operators)
   (left-binary-tier self operand operators
     (node BinaryExpression) (fields left operator right)))
  ((_ self operand operators (node kind) (fields lhs op rhs))
   (choice
    (alias kind
      (seq (field lhs (reference self))
           (field op operators)
           (field rhs operand)))
    operand)))

(defrules right-binary-tier (node fields)
  ((_ self operand operators)
   (right-binary-tier self operand operators
     (node BinaryExpression) (fields left operator right)))
  ((_ self operand operators (node kind) (fields lhs op rhs))
   (choice
    (alias kind
      (seq (field lhs operand)
           (field op operators)
           (field rhs (reference self))))
    operand)))

;;; FHIRPath-style chain: the alias also wraps the zero-operator case.
(defrules recursive-right-chain (node fields)
  ((_ self operand operators)
   (recursive-right-chain self operand operators
     (node BinaryExpression) (fields left operator right)))
  ((_ self operand operators (node kind) (fields lhs op rhs))
   (alias kind
     (seq (field lhs operand)
          (optional
           (seq (field op operators)
                (field rhs (reference self))))))))

;;; Only the design templates above expand here. Core expressions are interpreted
;;; by the repository's actual GrammarExpr constructors, not a research parser.
(defsyntax (vocabulary-value stx)
  (syntax-case stx ()
    ((_ expression)
     (let loop ((form #'expression) (steps 0))
       (when (> steps 4) (raise-syntax-error #f "unexpected template depth" stx))
       (syntax-case form (prec alias choice)
         ((prec . _) (with-syntax ((core form)) #'(grammar-expression core)))
         ((choice . _) (with-syntax ((core form)) #'(grammar-expression core)))
         ((alias . _) (with-syntax ((core form)) #'(grammar-expression core)))
         (_ (loop (core-expand1 form) (+ steps 1))))))))
