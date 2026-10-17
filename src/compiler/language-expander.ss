;;; -*- Gerbil -*-
;;; Engine-only expansion of admitted canonical declarations.
;;; Author grammars use language-support/grammar; this module owns IR fixtures
;;; and source-format compiler products, not a second author grammar interface.
(import (for-syntax (only-in ./parser-ir compile-parser)
                    (only-in ./bound-ir bind-grammar-ir)
                    (only-in ./language-artifact expand-language-grammar-syntax))
        (only-in ../language/descriptor make-language-grammar)
        (only-in ../language/assembly assemble-language-parser))
(export compile-language)
(defsyntax (compile-language stx)
  (expand-language-grammar-syntax
   stx compile-parser bind-grammar-ir
   #'assemble-language-parser #'make-language-grammar #'begin #'def))
