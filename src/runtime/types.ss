;;; Structural artifacts cross source and compiled execution boundaries.
(import (only-in :clan/poo/object .o .cc)
        (only-in :clan/poo/mop Function Any Bool)
        (only-in :clan/poo/type Record String)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(export RuntimeScopeContract)
(def RuntimeScopeContract
  (.cc YuanxuContract. identity: 'runtime/scope proto: (.o)
       value-contract: (Record
        parse-artifact-valid?: (list (Function (list Bool) (list Any)))
        parse-artifact-success?: (list (Function (list Bool) (list Any)))
        parse-artifact-roundtrip: (list (Function (list String) (list Any))))))
