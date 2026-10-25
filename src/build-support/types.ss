;;; Public build-support service contract; Scheme syntax stays in its existing facade.
(import (only-in :clan/poo/object .o .cc)
        (only-in :clan/poo/mop Function Any Bool)
        (only-in :clan/poo/type Record)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(export BuildSupportScopeContract)
(def BuildSupportScopeContract
  (.cc YuanxuContract. identity: 'build-support/scope proto: (.o)
       value-contract: (Record
        initialize-build!: (list (Function (list Any) '()))
        compiler-gsc-options: (list (Function (list Any) '()))
        compiler-build-spec: (list (Function (list Any) (list Any)))
        native-module-link-options: (list (Function (list Any) (list Any))))))
