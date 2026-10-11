;;; Author vocabulary entry; profiles and execution recipes are compiler products.
(import (only-in ./shell-grammar compile-shell-grammar)
        (only-in ../src/language/source declare-source-syntax)
        (for-syntax (only-in ../src/compiler/source-language expand-shell-grammar-syntax)))
(export shell)
(defsyntax (shell stx)
  (syntax-case stx (syntax rules)
    ((_ binding (syntax clause ...) (rules rule ...))
     (identifier? #'binding)
     (with-syntax ((strategy
                    (expand-shell-grammar-syntax #'(shell clause ... (rules rule ...))
                                                 #'compile-shell-grammar)))
       #'(def binding (declare-source-syntax strategy))))
    (_ (raise-syntax-error #f "shell vocabulary requires syntax and rules blocks" stx))))
