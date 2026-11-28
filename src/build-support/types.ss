;;; The native Record owns field types, inheritance, defaults and normalization.
(import (only-in :clan/poo/object .cc)
        (only-in :clan/poo/type define-type Fun Record String Symbol Bool List Or Pair Keyword)
        (only-in :clan/poo/number Integer))
(export BuildSupportScope BuildSupportScopeContract BuildOptionDatum BuildSpecification)

;; std/make owns option/metadata semantics, including nested submodule specs.
;; Constrain forwarded option data to structured Scheme atoms/lists without Any.
(define-type BuildOptionDatum
  (Or Symbol Keyword String Bool Integer (List String)))

(define-type BuildSpecification
  (List (Or String (Pair Keyword (Pair String (List BuildOptionDatum))))))

(define-type BuildSupportScope
  (Record compiler-gsc-options: [(Fun (List String))]
          compiler-build-spec: [(Fun BuildSpecification <- BuildSpecification)]
          native-module-link-options: [(Fun String <- (List String))]))

(def BuildSupportScopeContract
  (.cc BuildSupportScope identity: 'build-support/scope))
