;;; -*- Gerbil -*-
;;; incremental: state owner. Private execution state and named algorithms.

(export +edit-schema+
        +incremental-receipt-schema+
        +checkpoint-spacing+
        current-lr-fragment-reuse-enabled?
        current-lr-source-index-enabled?
        incremental-session-state
        make-incremental-session-state
        incremental-session-state?
        incremental-session-state-machine
        incremental-session-state-machine-set!
        incremental-session-state-source
        incremental-session-state-source-set!
        incremental-session-state-artifact
        incremental-session-state-artifact-set!
        incremental-session-state-tokens
        incremental-session-state-tokens-set!
        incremental-session-state-modes
        incremental-session-state-modes-set!
        incremental-session-state-checkpoints
        incremental-session-state-checkpoints-set!
        incremental-session-state-capture?
        incremental-session-state-capture?-set!
        incremental-session-state-recognition-root
        incremental-session-state-recognition-root-set!
        incremental-session-state-source-index
        incremental-session-state-source-index-set!
        incremental-session?
        incremental-session-artifact
        incremental-session-source-index
        incremental-session-source-modes
        incremental-session-recognition-root
        edit-record
        make-edit-record
        edit-record?
        edit-record-schema
        edit-record-schema-set!
        edit-record-start-byte
        edit-record-start-byte-set!
        edit-record-delete-byte-length
        edit-record-delete-byte-length-set!
        edit-record-inserted-text
        edit-record-inserted-text-set!
        edit?
        edit-start-byte
        edit-delete-byte-length
        edit-inserted-text)

;;; -*- Gerbil -*-
;;; LR checkpoint resume with lexical-mode-certified token reuse.
(def +edit-schema+ "gerbil-parser.edit.v1")

(def +incremental-receipt-schema+
  "gerbil-parser.incremental-receipt.v1")

(def +checkpoint-spacing+ 32)

;;; Experimental capture sessions can compare the same executor with transfers
;;; enabled/disabled. Ordinary sessions still do not capture a grammar forest.
(def current-lr-fragment-reuse-enabled? (make-parameter #t))

;;; Same-head native measurements currently favor vector convergence for full
;;; ParseArtifact edits. Persistent provenance remains an explicit experiment
;;; for the later local-recognition/publication boundary (RFC 0011).
(def current-lr-source-index-enabled? (make-parameter #f))

(defstruct incremental-session-state
  (machine source artifact tokens modes checkpoints capture? recognition-root source-index)
  transparent: #t)

(def incremental-session? incremental-session-state?)

(def incremental-session-artifact incremental-session-state-artifact)

(def incremental-session-source-index incremental-session-state-source-index)

(def incremental-session-source-modes incremental-session-state-modes)

;;; Private session execution metadata for conformance and future reuse.
;;; ParseArtifact remains the only published parse result.
(def incremental-session-recognition-root incremental-session-state-recognition-root)

;; : (-> String Nat Nat String EditRecord)
(defstruct edit-record (schema start-byte delete-byte-length inserted-text)
  transparent: #t)

;; : (-> Datum Boolean)
(def edit? edit-record?)

;; : (-> Edit Nat)
(def edit-start-byte edit-record-start-byte)

;; : (-> Edit Nat)
(def edit-delete-byte-length edit-record-delete-byte-length)

;; : (-> Edit String)
(def edit-inserted-text edit-record-inserted-text)

