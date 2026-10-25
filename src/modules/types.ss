;;; Public modules service contract; Scheme syntax stays in its existing facade.
(import (only-in :clan/poo/object .o .cc)
        (only-in :clan/poo/mop Function Any Bool)
        (only-in :clan/poo/type Record)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(export ParserModulesScopeContract)
(def ParserModulesScopeContract
  (.cc YuanxuContract. identity: 'modules/scope proto: (.o)
       value-contract: (Record
        make-graph-query-context: (list (Function (list Any) (list Any)))
        graph-query-view?: (list (Function (list Bool) (list Any))))))
