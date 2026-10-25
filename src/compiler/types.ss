;;; Public compiler service contract; Scheme syntax stays in its existing facade.
(import (only-in :clan/poo/object .o .cc)
        (only-in :clan/poo/mop Function Any Bool)
        (only-in :clan/poo/type Record)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(export CompilerScopeContract)
(def CompilerScopeContract
  (.cc YuanxuContract. identity: 'compiler/scope proto: (.o)
       value-contract: (Record
        compile-grammar: (list (Function (list Any) (list Any))))))
