;;; The native Record owns field types, inheritance, defaults and normalization.
(import (only-in :clan/poo/object .cc .get)
        (only-in :clan/poo/mop define-type Function Any Bool)
        (only-in :clan/poo/type Record List Symbol)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(export TestingScope TestingScopeContract)

;; Any is inspected syntax/reader data, never executed by this admission walk.
(define-type TestingScope
  (Record parser-test-form-admitted?: [(Function [Bool] [Any (List Symbol)] optional: [(List Symbol)])]))

;; Core adds contextual admission/evidence; it does not redeclare the fields.
(def TestingScopeContract
  (.cc YuanxuContract. identity: 'testing/scope
       proto: (.get TestingScope proto) value-contract: TestingScope))
