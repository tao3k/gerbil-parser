;;; ANTLR4 author entry; compiled consumers do not initialize its compiler.
(import (only-in ../src/grammar/lexical-algebra expand-text-lexical-rows)
        (for-syntax (only-in ../src/language/expansion-service resolve-language-expansion))
        (for-template (only-in ../src/compiler/antlr4-declaration
                               expand-antlr4-language expand-antlr4-vocabulary))
        (only-in ./antlr4-source antlr4-source-from-datum)
        (only-in ../src/runtime/language-artifact load-compiled-language-artifact/embedded)
        (only-in ../src/language/assembly assemble-language-parser)
        (only-in ../src/language/descriptor make-language-grammar))
(export antlr4)

(defsyntax (compile-antlr4-language stx)
  ((resolve-language-expansion ':gerbil-parser/src/compiler/antlr4-declaration 'expand-antlr4-language)
   stx #'(identity reference digest source entrypoint conflicts case-insensitive lexical-profile
          token-bindings syntax-kinds terminals lexical-rules extras)
   #'compile-antlr4-language #'antlr4-source-from-datum
   #'load-compiled-language-artifact/embedded #'assemble-language-parser
   #'make-language-grammar #'begin #'def))

(defsyntax (antlr4 stx)
  ((resolve-language-expansion ':gerbil-parser/src/compiler/antlr4-declaration 'expand-antlr4-vocabulary)
   stx #'(syntax rules reference digest source entrypoint conflicts case-insensitive token-bindings lex extras)
   #'compile-antlr4-language expand-text-lexical-rows))
