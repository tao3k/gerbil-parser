;;; Author vocabulary entry; profiles and execution recipes are compiler products.
(import (only-in ./shell-grammar compile-shell-grammar)
        (for-syntax (only-in ../src/compiler/source-language expand-shell-grammar-syntax)))
(export shell)
(defsyntax (shell stx)
  (expand-shell-grammar-syntax stx #'compile-shell-grammar))
