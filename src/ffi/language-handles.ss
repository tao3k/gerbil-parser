;;; -*- Gerbil -*-
;;; Language-independent ABI admission. All calls belong to one runtime thread.
(import (only-in ./language-abi-context language-abi-context?
                 language-abi-context-source-admission language-abi-context-event-row-writer)
        (only-in ./language-artifact-codec bind-language-abi
                 abi-parse-binary-payload/bytes/session abi-descriptor-payload)
        (only-in ../language/source-parser source-language-session-scanned-token-count
                 source-language-session-reused-token-count))
(export register-language! register-language-context! release-language!
        language-handle-descriptor language-handle-parse
        language-handle-parse/publish
        language-handle-scan-counts
        language-handle-owner! language-handle-count +language-source-byte-limit+)
(def +language-source-byte-limit+ 67108864)
(def +owner-thread+ #f)
(def +next-handle+ 0)
(def +languages+ (make-hash-table-eqv))
(defstruct language-handle-entry (language history generation))
(def (language-handle-owner!)
  (unless +owner-thread+ (set! +owner-thread+ (current-thread)))
  (unless (eq? +owner-thread+ (current-thread))
    (error "language ABI handle requires its runtime owner thread")))
(def (language-handle-count)
  (language-handle-owner!)
  (hash-length +languages+))
(def (register-language! descriptor (contextual-product #f))
  (register-language-context! (bind-language-abi descriptor contextual-product)))
;;; Prepared contexts retain their decoder and publication catalogs per handle.
(def (register-language-context! language)
  (language-handle-owner!)
  (unless (and (language-abi-context? language)
               (procedure? (language-abi-context-source-admission language))
               (procedure? (language-abi-context-event-row-writer language)))
    (error "expected prepared language ABI context"))
  (when (>= +next-handle+ 18446744073709551615)
    (error "language ABI handle space exhausted"))
  (set! +next-handle+ (+ +next-handle+ 1))
  (hash-put! +languages+ +next-handle+ (make-language-handle-entry language #f 0))
  +next-handle+)
(def (release-language! handle)
  (language-handle-owner!)
  (hash-remove! +languages+ handle))
(def (admit-handle handle)
  (language-handle-owner!)
  (or (hash-get +languages+ handle)
      (error "unknown or released language ABI handle" handle)))
(def (language-handle-descriptor handle)
  (abi-descriptor-payload (language-handle-entry-language (admit-handle handle))))
(def (language-handle-scan-counts handle)
  (let (history (language-handle-entry-history (admit-handle handle)))
    (values (and history (source-language-session-scanned-token-count history))
            (if history (source-language-session-reused-token-count history) 0))))
(def (language-handle-parse handle bytes)
  (language-handle-parse/publish handle bytes (lambda (_) #t)))
;;; The C owner commits history only after allocating and copying its result.
(def (language-handle-parse/publish handle bytes publish)
  (let (entry (admit-handle handle))
    (unless (and (u8vector? bytes)
                 (<= (u8vector-length bytes) +language-source-byte-limit+))
      (error "invalid or oversized ABI source bytes"))
    ;; Advance before recognition: even a failed newer attempt supersedes the
    ;; older callback. Exact integers do not wrap back to an earlier ticket.
    (let (generation (+ 1 (language-handle-entry-generation entry)))
     (language-handle-entry-generation-set! entry generation)
     (let-values (((payload history)
                  (abi-parse-binary-payload/bytes/session
                   (language-handle-entry-language entry) bytes
                   (language-handle-entry-history entry))))
      (when (and (publish payload)
                 (= generation (language-handle-entry-generation entry))
                 (eq? entry (hash-get +languages+ handle)))
        (language-handle-entry-history-set! entry history))
      payload))))
