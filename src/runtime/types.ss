;;; The native Record owns field types, inheritance, defaults and normalization.
(import (only-in :clan/poo/object .cc .get)
        (only-in :clan/poo/mop define-type Fun Any Bool Type.)
        (only-in :clan/poo/type Record String)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(import (only-in "./artifact.ss" parse-artifact-valid?))
(export ParseArtifact RuntimeScope RuntimeScopeContract)

;; Reuse the authoritative structural validator across source/compiled worlds.
(define-type (ParseArtifact @ Type.) .element?: parse-artifact-valid?)

(define-type RuntimeScope
  (Record parse-artifact-valid?: [(Fun Bool <- Any)]
          parse-artifact-success?: [(Fun Bool <- ParseArtifact)]
          parse-artifact-roundtrip: [(Fun String <- ParseArtifact)]))

;; Core adds contextual admission/evidence; it does not redeclare the fields.
(def RuntimeScopeContract
  (.cc YuanxuContract. identity: 'runtime/scope
       proto: (.get RuntimeScope proto) value-contract: RuntimeScope))
