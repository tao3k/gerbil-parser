;;; The native Record owns field types, inheritance, defaults and normalization.
(import (only-in :clan/poo/object .cc .get)
        (only-in :clan/poo/mop define-type)
        (only-in :clan/poo/type Record Exactly)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(import (only-in ../schema +language-abi-version+ +artifact-abi-version+
                 +event-abi-version+ +rust-aot-abi-version+))
(export ForeignInterfaceScope ForeignInterfaceScopeContract)

(define-type ForeignInterfaceScope
  (Record language-abi-version: [(Exactly +language-abi-version+)]
          artifact-abi-version: [(Exactly +artifact-abi-version+)]
          event-abi-version: [(Exactly +event-abi-version+)]
          rust-aot-abi-version: [(Exactly +rust-aot-abi-version+)]))

;; Core adds contextual admission/evidence; it does not redeclare the fields.
(def ForeignInterfaceScopeContract
  (.cc YuanxuContract. identity: 'ffi/scope
       proto: (.get ForeignInterfaceScope proto) value-contract: ForeignInterfaceScope))
