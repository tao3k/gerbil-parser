;;; The native Record owns field types, inheritance, defaults and normalization.
(import (only-in :clan/poo/object .cc)
        (only-in :clan/poo/type define-type Function Any Bool Record List Symbol))
(export TestingScope TestingScopeContract)

;; Any is inspected syntax/reader data, never executed by this admission walk.
(define-type TestingScope
  (Record parser-test-form-admitted?: [(Function [Bool] [Any (List Symbol)] optional: [(List Symbol)])]))

(def TestingScopeContract
  (.cc TestingScope identity: 'testing/scope))
