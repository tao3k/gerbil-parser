;;; -*- Gerbil -*-
;;; Language-independent native admission. All calls belong to one runtime thread.
(import (only-in ./language-artifact-codec bind-native-language
                 native-parse-binary-payload/bytes/session native-descriptor-payload)
        (only-in ../language/source source-language-session-scanned-token-count
                 source-language-session-reused-token-count))
(export register-native-language! release-native-language!
        native-language-handle-descriptor native-language-handle-parse
        native-language-handle-parse/publish
        native-language-handle-scan-counts
        native-language-handle-owner! native-language-handle-count +native-source-byte-limit+)
(def +native-source-byte-limit+ 67108864)
(def +owner-thread+ #f)
(def +next-handle+ 0)
(def +languages+ (make-hash-table-eqv))
(defstruct native-language-entry (language history))
(def (native-language-handle-owner!)
  (unless +owner-thread+ (set! +owner-thread+ (current-thread)))
  (unless (eq? +owner-thread+ (current-thread))
    (error "native language handle requires its runtime owner thread")))
(def (native-language-handle-count)
  (native-language-handle-owner!)
  (hash-length +languages+))
(def (register-native-language! descriptor (contextual-product #f))
  (native-language-handle-owner!)
  (let (language (bind-native-language descriptor contextual-product))
    (when (>= +next-handle+ 18446744073709551615)
      (error "native language handle space exhausted"))
    (set! +next-handle+ (+ +next-handle+ 1))
    (hash-put! +languages+ +next-handle+ (make-native-language-entry language #f))
    +next-handle+))
(def (release-native-language! handle)
  (native-language-handle-owner!)
  (hash-remove! +languages+ handle))
(def (admit-handle handle)
  (native-language-handle-owner!)
  (or (hash-get +languages+ handle)
      (error "unknown or released native language handle" handle)))
(def (native-language-handle-descriptor handle)
  (native-descriptor-payload (native-language-entry-language (admit-handle handle))))
(def (native-language-handle-scan-counts handle)
  (let (history (native-language-entry-history (admit-handle handle)))
    (values (and history (source-language-session-scanned-token-count history))
            (if history (source-language-session-reused-token-count history) 0))))
(def (native-language-handle-parse handle bytes)
  (native-language-handle-parse/publish handle bytes (lambda (_) #t)))
;;; The C owner commits history only after allocating and copying its result.
(def (native-language-handle-parse/publish handle bytes publish)
  (let (entry (admit-handle handle))
    (unless (and (u8vector? bytes)
                 (<= (u8vector-length bytes) +native-source-byte-limit+))
      (error "invalid or oversized native source bytes"))
    (let-values (((payload history)
                  (native-parse-binary-payload/bytes/session
                   (native-language-entry-language entry) bytes
                   (native-language-entry-history entry))))
      (when (publish payload) (native-language-entry-history-set! entry history))
      payload)))
