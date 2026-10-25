;;; The native Record owns field types, inheritance, defaults and normalization.
(import (only-in :clan/poo/object .cc .get)
        (only-in :clan/poo/mop define-type Fun Any Bool)
        (only-in :clan/poo/type Record String)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(export RuntimeScope RuntimeScopeContract)

(define-type RuntimeScope
  (Record parse-artifact-valid?: [(Fun Bool <- Any)]
          parse-artifact-success?: [(Fun Bool <- Any)]
          parse-artifact-roundtrip: [(Fun String <- Any)]))

;; Core adds contextual admission/evidence; it does not redeclare the fields.
(def RuntimeScopeContract
  (.cc YuanxuContract. identity: 'runtime/scope
       proto: (.get RuntimeScope proto) value-contract: RuntimeScope))
