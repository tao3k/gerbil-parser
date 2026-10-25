;;; ABI versions are owned by schema.ss; this contract only describes the view.
(import (only-in :clan/poo/object .o .cc)
        (only-in :clan/poo/type Record)
        (only-in :clan/poo/number Integer)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(export ForeignInterfaceScopeContract)
(def ForeignInterfaceScopeContract
  (.cc YuanxuContract. identity: 'ffi/scope proto: (.o)
       value-contract: (Record language-abi-version: (list Integer)
                              artifact-abi-version: (list Integer)
                              event-abi-version: (list Integer)
                              rust-aot-abi-version: (list Integer))))
