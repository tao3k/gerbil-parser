;;; The native Record owns field types, inheritance, defaults and normalization.
(import (only-in :clan/poo/object .cc .get)
        (only-in :clan/poo/mop define-type Fun Any Bool)
        (only-in :clan/poo/type Record)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(export ParserModulesScope ParserModulesScopeContract)

(define-type ParserModulesScope
  (Record make-graph-query-context: [(Fun Any <- Any)]
          graph-query-view?: [(Fun Bool <- Any)]))

;; Core adds contextual admission/evidence; it does not redeclare the fields.
(def ParserModulesScopeContract
  (.cc YuanxuContract. identity: 'modules/scope
       proto: (.get ParserModulesScope proto) value-contract: ParserModulesScope))
