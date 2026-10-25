;;; Public testing service contract; Scheme syntax stays in its existing facade.
(import (only-in :clan/poo/object .o .cc)
        (only-in :clan/poo/mop Function Any Bool)
        (only-in :clan/poo/type Record)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(export TestingScopeContract)
(def TestingScopeContract
  (.cc YuanxuContract. identity: 'testing/scope proto: (.o)
       value-contract: (Record
        parser-test-form-admitted?: (list (Function (list Bool) (list Any Any) optional: (list Any))))))
