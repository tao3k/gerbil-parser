;;; ISO BNF author entry; compiled consumers request only runtime products.
(import (for-syntax (only-in ../src/language/expansion-service resolve-language-expansion))
        (for-template (only-in ../src/compiler/iso-bnf-declaration
                               expand-iso-bnf-language expand-iso-bnf-vocabulary))
        (only-in ../src/compiler/language-expander compile-language))
(export iso-bnf)

(defsyntax (compile-iso-bnf-language stx)
  ((resolve-language-expansion ':gerbil-parser/src/compiler/iso-bnf-declaration 'expand-iso-bnf-language)
   stx #'(identity reference digest source rule-overrides rule-precedences entrypoint conflicts case-insensitive)
   #'compile-iso-bnf-language #'compile-language))

(defsyntax (iso-bnf stx)
  ((resolve-language-expansion ':gerbil-parser/src/compiler/iso-bnf-declaration 'expand-iso-bnf-vocabulary)
   stx #'(syntax rules reference digest source rule-precedences entrypoint conflicts case-insensitive)
   #'compile-iso-bnf-language))
