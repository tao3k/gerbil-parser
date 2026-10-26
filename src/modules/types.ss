;;; The native Record owns field types, inheritance, defaults and normalization.
(import (only-in :clan/poo/object .cc .get)
        (only-in :clan/poo/mop define-type Fun Any Bool Type Object)
        (only-in :clan/poo/type Record)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(import (only-in "./parser/graph-query-types.ss" GraphQueryContextContract))
(export ParserModulesScope ParserModulesScopeContract)

;; The last view? argument is an arbitrary candidate; Row/Id/Kind remain explicit Types.
(define-type ParserModulesScope
  (Record make-graph-query-context: [(Fun GraphQueryContextContract <- Type Type Type Object)]
          graph-query-view?: [(Fun Bool <- Type Type Type Any)]))

;; Core adds contextual admission/evidence; it does not redeclare the fields.
(def ParserModulesScopeContract
  (.cc YuanxuContract. identity: 'modules/scope
       proto: (.get ParserModulesScope proto) value-contract: ParserModulesScope))
