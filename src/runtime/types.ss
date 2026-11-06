;;; The native Record owns field types, inheritance, defaults and normalization.
(import (only-in :clan/poo/object .cc)
        (only-in :clan/poo/mop define-type Fun Any Bool Type.)
        (only-in :clan/poo/type Record String))
(import (only-in "./artifact.ss" parse-artifact-valid?))
(export ParseArtifact RuntimeScope RuntimeScopeContract)

;; Reuse the authoritative structural validator across source/compiled worlds.
(define-type (ParseArtifact @ Type.) .element?: parse-artifact-valid?)

;; Only valid? accepts Any: it must classify malformed candidates as #f.
(define-type RuntimeScope
  (Record parse-artifact-valid?: [(Fun Bool <- Any)]
          parse-artifact-success?: [(Fun Bool <- ParseArtifact)]
          parse-artifact-roundtrip: [(Fun String <- ParseArtifact)]))

(def RuntimeScopeContract
  (.cc RuntimeScope identity: 'runtime/scope))
