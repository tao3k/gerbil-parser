;;; The native Record owns field types, inheritance, defaults and normalization.
(import (only-in :clan/poo/object .cc)
        (only-in :clan/poo/mop define-type)
        (only-in :clan/poo/type Record Exactly))
(import (only-in ../schema +grammar-schema+ +language-grammar-schema+))
(export LanguageScope LanguageScopeContract)

(define-type LanguageScope
  (Record grammar-schema: [(Exactly +grammar-schema+)]
          language-grammar-schema: [(Exactly +language-grammar-schema+)]))

(def LanguageScopeContract
  (.cc LanguageScope identity: 'language/scope))
