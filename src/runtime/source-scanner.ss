;;; -*- Gerbil -*-
;;; Immutable source-scanner state for languages whose lexical context changes
;;; across token boundaries. A state is a reusable checkpoint for one source.

(import (only-in :std/string/utf8 string-utf8-length)
        (only-in ./token make-token token-kind token-lexeme token-start token-end)
        (only-in ./contextual-scanner prepare-contextual-scanner contextual-scanner-initial-state
                 contextual-scanner-step contextual-scan-state-character-offset contextual-scan-state-byte-offset
                 contextual-scan-state-source-length contextual-scan-state-common-suffix-length
                 prepare-contextual-scan-suffix contextual-scan-suffix-state))
(export source-scanner-contextual? source-scanner-driver? source-scanner-for-source? source-scanner-tokens make-source-scanner
        make-contextual-source-scanner source-scanner-initial-state
        source-scanner-step
        source-scan-state?
        source-scan-state-character-offset
        source-scan-state-byte-offset
        source-scan-state-context
        source-scan-state-with-context
        make-source-scan-session source-scan-session? source-scan-session-tokens
        source-scan-session-scanned-token-count source-scan-session-reused-token-count)

(defstruct source-scanner-driver (source initial-context scan) transparent: #t)
(defstruct (contextual-source-driver source-scanner-driver) (executor position))
(def source-scanner-contextual? contextual-source-driver?)
(def (make-contextual-source-scanner plan source position)
  (let* ((executor (prepare-contextual-scanner plan source))
         (initial (contextual-scanner-initial-state executor)))
    (make-contextual-source-driver source initial #f executor position)))

(defstruct source-scan-state (owner source character-offset byte-offset context)
  transparent: #t)

(def (make-source-scanner source initial-context step)
  (unless (and (string? source) (procedure? step))
    (error "source scanner requires source and scan procedure"))
  (make-source-scanner-driver source initial-context step))

(def (source-scanner-for-source? scanner source)
  (and (source-scanner-driver? scanner)
       (eq? source (source-scanner-driver-source scanner))))

(def (source-scanner-initial-state scanner)
  (make-source-scan-state scanner (source-scanner-driver-source scanner) 0 0
                          (source-scanner-driver-initial-context scanner)))

;;; A language may update its immutable context when a parser reduction
;;; admits a deferred lexical obligation, without changing source position.
(def (source-scan-state-with-context state context)
  (make-source-scan-state
   (source-scan-state-owner state) (source-scan-state-source state)
   (source-scan-state-character-offset state)
   (source-scan-state-byte-offset state)
   context))

(def (step-contextual-source scanner state)
  (let (context (source-scan-state-context state))
    (unless (and (= (contextual-scan-state-character-offset context)
                    (source-scan-state-character-offset state))
                 (= (contextual-scan-state-byte-offset context)
                    (source-scan-state-byte-offset state)))
      (error "contextual source checkpoint offsets disagree"))
    (let-values (((token next)
                  (contextual-scanner-step
                   (contextual-source-driver-executor scanner) context
                   (contextual-source-driver-position scanner))))
      (values token
              (make-source-scan-state scanner (source-scanner-driver-source scanner)
               (contextual-scan-state-character-offset next)
               (contextual-scan-state-byte-offset next) next)))))

;;; Engine callbacks return (values kind exclusive-character-end context).
;;; Contextual workers forward the token and offsets owned by the shared IR executor.
;;; Ordinary workers check progress and construct source-backed byte spans.
(def (source-scanner-step scanner state mode)
  (unless (and (source-scan-state? state)
               (eq? scanner (source-scan-state-owner state)))
    (error "scanner checkpoint belongs to another worker"))
  (if (contextual-source-driver? scanner)
    (step-contextual-source scanner state)
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
                    (make-source-scan-state scanner source end byte-end context))))
        (begin
          (unless (= start length)
            (error "source scanner stopped before EOF" start length))
          (values #f (source-scan-state-with-context state context))))))))

;;; A scan worker drains its own immutable checkpoints; traversal and token
;;; ownership stay here for both engine callbacks and closed contextual profiles.
(def (source-scanner-tokens scanner mode)
  (unless (source-scanner-driver? scanner) (error "invalid source scanner worker"))
  (let loop ((state (source-scanner-initial-state scanner)) (tokens '()))
    (let-values (((token next) (source-scanner-step scanner state mode)))
      (if token (loop next (cons token tokens)) (reverse tokens)))))

;;; Sessions own token records; callers receive copies so publication or edits
;;; cannot corrupt a future reuse candidate. Checkpoint vectors stay private.
(defstruct source-scan-session-state (worker mode tokens-field states scanned-token-count reused-token-count))
(def source-scan-session? source-scan-session-state?)
(def source-scan-session-scanned-token-count source-scan-session-state-scanned-token-count)
(def source-scan-session-reused-token-count source-scan-session-state-reused-token-count)
(def (copy-session-token token (delta 0))
  (make-token (token-kind token) (string-copy (token-lexeme token))
              (+ (token-start token) delta) (+ (token-end token) delta)))
(def (source-scan-session-tokens session)
  (map copy-session-token (source-scan-session-state-tokens-field session)))

(def (source-session-state-index states character)
  (let loop ((low 0) (high (vector-length states)))
    (if (>= low high) #f
      (let* ((middle (quotient (+ low high) 2))
             (offset (source-scan-state-character-offset (vector-ref states middle))))
        (cond ((= offset character) middle)
              ((< offset character) (loop (+ middle 1) high))
              (else (loop low middle)))))))

(def (make-source-scan-session worker mode (previous #f))
  (unless (and (source-scanner-driver? worker)
               (or (not previous) (source-scan-session? previous)))
    (error "invalid source scanning session"))
  (let* ((initial (source-scanner-initial-state worker))
         (old-worker (and previous (source-scan-session-state-worker previous)))
         (eligible? (and previous (contextual-source-driver? worker)
                         (contextual-source-driver? old-worker)
                         (eq? mode (source-scan-session-state-mode previous))
                         (eq? (contextual-source-driver-position worker)
                              (contextual-source-driver-position old-worker))))
         (old-states (and eligible? (source-scan-session-state-states previous)))
         (old-context (and eligible? (source-scan-state-context (vector-ref old-states 0))))
         (new-context (source-scan-state-context initial))
         (delta (and eligible? (- (contextual-scan-state-source-length new-context)
                                  (contextual-scan-state-source-length old-context))))
         (suffix-start
          (and eligible?
               (- (contextual-scan-state-source-length new-context)
                  (contextual-scan-state-common-suffix-length old-context new-context)))))
    (let loop ((state initial) (tokens '()) (states (list initial)) (scanned 0))
      (let* ((index (and eligible?
                         (>= (source-scan-state-character-offset state) suffix-start)
                         (source-session-state-index old-states
                          (- (source-scan-state-character-offset state) delta))))
             (before (and index (vector-ref old-states index)))
             (suffix (and before
                          (prepare-contextual-scan-suffix
                           (source-scan-state-context before) (source-scan-state-context state)))))
        (if suffix
          (let* ((byte-delta (- (source-scan-state-byte-offset state)
                               (source-scan-state-byte-offset before)))
                 (tail (list-tail (source-scan-session-state-tokens-field previous) index))
                 (reused (map (lambda (token) (copy-session-token token byte-delta)) tail))
                 (rebound
                  (map (lambda (old)
                         (let (context (contextual-scan-suffix-state suffix (source-scan-state-context old)))
                           (make-source-scan-state worker (source-scanner-driver-source worker)
                            (contextual-scan-state-character-offset context)
                            (contextual-scan-state-byte-offset context) context)))
                       (list-tail (vector->list old-states) (+ index 1)))))
            (make-source-scan-session-state worker mode (append (reverse tokens) reused)
             (list->vector (append (reverse states) rebound)) scanned (length reused)))
          (let-values (((token next) (source-scanner-step worker state mode)))
            (if token
              (loop next (cons token tokens) (cons next states) (+ scanned 1))
              (make-source-scan-session-state worker mode (reverse tokens)
               (list->vector (reverse states)) scanned 0))))))))
