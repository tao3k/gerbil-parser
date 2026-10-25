;;; The native Record owns field types, inheritance, defaults and normalization.
(import (only-in :clan/poo/object .cc .get)
        (only-in :clan/poo/mop define-type Function Any Bool)
        (only-in :clan/poo/type Record)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(export TestingScope TestingScopeContract)

(define-type TestingScope
  (Record parser-test-form-admitted?: [(Function [Bool] [Any Any] optional: [Any])]))

;; Core adds contextual admission/evidence; it does not redeclare the fields.
(def TestingScopeContract
  (.cc YuanxuContract. identity: 'testing/scope
       proto: (.get TestingScope proto) value-contract: TestingScope))
