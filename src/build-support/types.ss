;;; The native Record owns field types, inheritance, defaults and normalization.
(import (only-in :clan/poo/object .cc)
        (only-in :clan/poo/type define-type Type. Fun Record String List Or Pair Keyword))
(export BuildSupportScope BuildSupportScopeContract BuildOptionDatum BuildSpecification
        build-option-datum?)

;; std/make owns option/metadata semantics, including nested submodule specs
;; such as (submodules: ("child")). Constrain forwarded option data to
;; structured Scheme atoms/lists without Any.
(def (build-option-datum? value)
  (or (null? value)
      (symbol? value)
      (keyword? value)
      (string? value)
      (boolean? value)
      (exact-integer? value)
      (and (pair? value)
           (build-option-datum? (car value))
           (build-option-datum? (cdr value)))))

(define-type (BuildOptionDatum @ Type.)
  .element?: build-option-datum?)

(define-type BuildSpecification
  (List (Or String (Pair Keyword (Pair String (List BuildOptionDatum))))))

(define-type BuildSupportScope
  (Record compiler-gsc-options: [(Fun (List String))]
          compiler-build-spec: [(Fun BuildSpecification <- BuildSpecification)]
          native-module-link-options: [(Fun String <- (List String))]))

(def BuildSupportScopeContract
  (.cc BuildSupportScope identity: 'build-support/scope))
