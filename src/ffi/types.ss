;;; The native Record owns field types, inheritance, defaults and normalization.
(import (only-in :clan/poo/object .cc .get)
        (only-in :clan/poo/mop define-type)
        (only-in :clan/poo/type Record)
        (only-in :clan/poo/number Integer)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(export ForeignInterfaceScope ForeignInterfaceScopeContract)

(define-type ForeignInterfaceScope
  (Record language-abi-version: [Integer]
          artifact-abi-version: [Integer]
          event-abi-version: [Integer]
          rust-aot-abi-version: [Integer]))

;; Core adds contextual admission/evidence; it does not redeclare the fields.
(def ForeignInterfaceScopeContract
  (.cc YuanxuContract. identity: 'ffi/scope
       proto: (.get ForeignInterfaceScope proto) value-contract: ForeignInterfaceScope))
