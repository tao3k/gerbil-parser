;;; -*- Gerbil -*-
;;; Immutable source-scanner state for languages whose lexical context changes
;;; across token boundaries. A state is a reusable checkpoint for one source.

(import (only-in :std/string/utf8 string-utf8-length)
        (only-in ./token make-token))
(export make-source-scanner
        source-scanner-initial-state
        source-scanner-step
        source-scan-state?
        source-scan-state-character-offset
        source-scan-state-byte-offset
        source-scan-state-context
        source-scan-state-with-context)

(defstruct source-scanner-driver (source initial-context scan) transparent: #t)
(defstruct source-scan-state (source character-offset byte-offset context)
  transparent: #t)

(def (make-source-scanner source initial-context step)
  (unless (and (string? source) (procedure? step))
    (error "source scanner requires source and scan procedure"))
  (make-source-scanner-driver source initial-context step))

(def (source-scanner-initial-state scanner)
  (make-source-scan-state (source-scanner-driver-source scanner) 0 0
                          (source-scanner-driver-initial-context scanner)))

;;; A language may update its immutable context when a parser reduction
;;; admits a deferred lexical obligation, without changing source position.
(def (source-scan-state-with-context state context)
  (make-source-scan-state
   (source-scan-state-source state)
   (source-scan-state-character-offset state)
   (source-scan-state-byte-offset state)
   context))

;;; The language callback returns (values kind exclusive-character-end context).
;;; The engine checks progress and owns byte offsets and token construction.
(def (source-scanner-step scanner state mode)
  (unless (and (source-scan-state? state)
               (eq? (source-scanner-driver-source scanner)
                    (source-scan-state-source state)))
    (error "scanner checkpoint belongs to another source"))
  (let* ((source (source-scanner-driver-source scanner))
         (start (source-scan-state-character-offset state))
         (length (string-length source)))
    (let-values (((kind end context)
                  ((source-scanner-driver-scan scanner)
                   source start (source-scan-state-context state) mode)))
      (if kind
        (begin
          (unless (and (symbol? kind) (integer? end)
                       (< start end) (<= end length))
            (error "source scanner did not advance" kind start end))
          (let* ((lexeme (substring source start end))
                 (byte-end (+ (source-scan-state-byte-offset state)
                              (string-utf8-length lexeme)))
                 (token (make-token kind lexeme
                                    (source-scan-state-byte-offset state)
                                    byte-end)))
            (values token
                    (make-source-scan-state source end byte-end context))))
        (begin
          (unless (= start length)
            (error "source scanner stopped before EOF" start length))
          (values #f (source-scan-state-with-context state context)))))))
