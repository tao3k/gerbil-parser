;;; The native Record owns field types, inheritance, defaults and normalization.
(import (only-in :clan/poo/object .cc .get)
        (only-in :clan/poo/mop define-type)
        (only-in :clan/poo/type Record String)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(export LanguageScope LanguageScopeContract)

(define-type LanguageScope
  (Record grammar-schema: [String]
          language-grammar-schema: [String]))

;; Core adds contextual admission/evidence; it does not redeclare the fields.
(def LanguageScopeContract
  (.cc YuanxuContract. identity: 'language/scope
       proto: (.get LanguageScope proto) value-contract: LanguageScope))
