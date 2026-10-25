;;; The native Record owns field types, inheritance, defaults and normalization.
(import (only-in :clan/poo/object .cc .get)
        (only-in :clan/poo/mop define-type Fun Any)
        (only-in :clan/poo/type Record)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(export BuildSupportScope BuildSupportScopeContract)

(define-type BuildSupportScope
  (Record initialize-build!: [(Fun Any)]
          compiler-gsc-options: [(Fun Any)]
          compiler-build-spec: [(Fun Any <- Any)]
          native-module-link-options: [(Fun Any <- Any)]))

;; Core adds contextual admission/evidence; it does not redeclare the fields.
(def BuildSupportScopeContract
  (.cc YuanxuContract. identity: 'build-support/scope
       proto: (.get BuildSupportScope proto) value-contract: BuildSupportScope))
