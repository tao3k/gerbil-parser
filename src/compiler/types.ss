;;; The native Record owns field types, inheritance, defaults and normalization.
(import (only-in :clan/poo/object .cc .get)
        (only-in :clan/poo/mop define-type Fun Any)
        (only-in :clan/poo/type Record)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(export CompilerScope CompilerScopeContract)

(define-type CompilerScope
  (Record compile-grammar: [(Fun Any <- Any)]))

;; Core adds contextual admission/evidence; it does not redeclare the fields.
(def CompilerScopeContract
  (.cc YuanxuContract. identity: 'compiler/scope
       proto: (.get CompilerScope proto) value-contract: CompilerScope))
