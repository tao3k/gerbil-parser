;;; Public grammar service contract; Scheme syntax stays in its existing facade.
(import (only-in :clan/poo/object .o .cc)
        (only-in :clan/poo/mop Function Any Bool)
        (only-in :clan/poo/type Record)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(export GrammarScopeContract)
(def GrammarScopeContract
  (.cc YuanxuContract. identity: 'grammar/scope proto: (.o)
       value-contract: (Record
        grammar-expression?: (list (Function (list Bool) (list Any)))
        lexical-expression?: (list (Function (list Bool) (list Any))))))
