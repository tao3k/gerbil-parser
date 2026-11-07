;;; The native Record owns field types, inheritance, defaults and normalization.
(import (only-in :clan/poo/object .cc)
        (only-in :clan/poo/mop define-type Fun Any Bool)
        (only-in :clan/poo/type Record))
(export GrammarScope GrammarScopeContract)

;; Any is the candidate domain: malformed values must yield #f, not fail argument admission.
(define-type GrammarScope
  (Record grammar-expression?: [(Fun Bool <- Any)]
          lexical-expression?: [(Fun Bool <- Any)]))

(def GrammarScopeContract
  (.cc GrammarScope identity: 'grammar/scope))
