;;; Format identities come from the existing schema declaration.
(import (only-in :clan/poo/object .o .cc)
        (only-in :clan/poo/type Record String)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(export LanguageScopeContract)
(def LanguageScopeContract
  (.cc YuanxuContract. identity: 'language/scope proto: (.o)
       value-contract: (Record grammar-schema: (list String)
                              language-grammar-schema: (list String))))
