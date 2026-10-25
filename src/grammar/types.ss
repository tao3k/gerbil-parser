;;; The native Record owns field types, inheritance, defaults and normalization.
(import (only-in :clan/poo/object .cc .get)
        (only-in :clan/poo/mop define-type Fun Any Bool)
        (only-in :clan/poo/type Record)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(export GrammarScope GrammarScopeContract)

(define-type GrammarScope
  (Record grammar-expression?: [(Fun Bool <- Any)]
          lexical-expression?: [(Fun Bool <- Any)]))

;; Core adds contextual admission/evidence; it does not redeclare the fields.
(def GrammarScopeContract
  (.cc YuanxuContract. identity: 'grammar/scope
       proto: (.get GrammarScope proto) value-contract: GrammarScope))
