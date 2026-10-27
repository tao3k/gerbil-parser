;;; The native Record owns field types, inheritance, defaults and normalization.
(import (only-in :clan/poo/object .cc .get)
        (only-in :clan/poo/mop define-type Fun Any)
        (only-in :clan/poo/type Record String List Or Pair Keyword)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(export BuildSupportScope BuildSupportScopeContract BuildSpecification)

;; std/make owns option/metadata semantics, including nested submodule specs.
;; Any is retained only for that forwarded std/make option tail, not the source name.
;; Recreating its option language here would duplicate the native build system.
(define-type BuildSpecification (List (Or String (Pair Keyword (Pair String (List Any))))))

(define-type BuildSupportScope
  (Record compiler-gsc-options: [(Fun (List String))]
          compiler-build-spec: [(Fun BuildSpecification <- BuildSpecification)]
          native-module-link-options: [(Fun String <- (List String))]))

;; Core adds contextual admission/evidence; it does not redeclare the fields.
(def BuildSupportScopeContract
  (.cc YuanxuContract. identity: 'build-support/scope
       proto: (.get BuildSupportScope proto) value-contract: BuildSupportScope))
