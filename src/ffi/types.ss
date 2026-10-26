;;; The native Record owns field types, inheritance, defaults and normalization.
(import (only-in :clan/poo/object .cc .get)
        (only-in :clan/poo/mop define-type)
        (only-in :clan/poo/type Record)
        (only-in :clan/poo/number UInt)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(export ForeignInterfaceScope ForeignInterfaceScopeContract)

(define-type ForeignInterfaceScope
  (Record language-abi-version: [UInt]
          artifact-abi-version: [UInt]
          event-abi-version: [UInt]
          rust-aot-abi-version: [UInt]))

;; Core adds contextual admission/evidence; it does not redeclare the fields.
(def ForeignInterfaceScopeContract
  (.cc YuanxuContract. identity: 'ffi/scope
       proto: (.get ForeignInterfaceScope proto) value-contract: ForeignInterfaceScope))
