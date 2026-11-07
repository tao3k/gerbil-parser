;;; The native Record owns field types, inheritance, defaults and normalization.
(import (only-in :clan/poo/object .cc)
        (only-in :clan/poo/mop define-type)
        (only-in :clan/poo/type Record Exactly))
(import (only-in ../schema +language-abi-version+ +artifact-abi-version+
                 +event-abi-version+ +rust-aot-abi-version+))
(export ForeignInterfaceScope ForeignInterfaceScopeContract)

(define-type ForeignInterfaceScope
  (Record language-abi-version: [(Exactly +language-abi-version+)]
          artifact-abi-version: [(Exactly +artifact-abi-version+)]
          event-abi-version: [(Exactly +event-abi-version+)]
          rust-aot-abi-version: [(Exactly +rust-aot-abi-version+)]))

(def ForeignInterfaceScopeContract
  (.cc ForeignInterfaceScope identity: 'ffi/scope))
