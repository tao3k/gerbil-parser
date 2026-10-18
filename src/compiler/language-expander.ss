;;; -*- Gerbil -*-
;;; Engine-only expansion of admitted canonical declarations.
;;; Author grammars use language-support/grammar; this module owns IR fixtures
;;; and source-format compiler products, not a second author grammar interface.
(import (for-syntax (only-in ../language/expansion-service resolve-language-expansion))
        (for-template (only-in ./language-declaration expand-canonical-language-declaration))
        (only-in ../language/descriptor make-language-grammar)
        (only-in ../language/assembly assemble-language-parser))
(export compile-language)
(defsyntax (compile-language stx)
  ((resolve-language-expansion ':gerbil-parser/src/compiler/language-declaration
                               'expand-canonical-language-declaration)
   stx
   #'assemble-language-parser #'make-language-grammar #'begin #'def))
