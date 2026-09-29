;;; -*- Gerbil -*-
;;; LR checkpoint resume with lexical-mode-certified token reuse.

(import (only-in ../compiler/machine
                 parser-machine-grammar-digest parser-machine-ir
                 parser-machine-runtime parser-machine-trivia)
        (only-in ../compiler/parser-ir parser-ir-ref)
        (only-in ./artifact
                 event-end event-start make-same-width-trivia-artifact
                 parse-artifact-events
                 parse-artifact-ref parse-artifact-success?
                 parse-artifact-valid? token-event?
                 token-event-lexeme token-event-token-kind)
        (only-in ./identity sha256-text)
        (only-in ./lr-parser
                 lr-lexical-mode-id lr-runtime-lexical-mode-catalog
                 lr-prefix-snapshot-rebind)
        (only-in ./lexer scan-source-token)
        (only-in ./parser
                 parse-source/checkpoints
                 parse-source/checkpoints/resume)
        (only-in ./significant parser-significant-tokens)
        (only-in ./token
                 make-token token-end token-kind token-lexeme token-start))
(export +edit-schema+
        +incremental-receipt-schema+
        make-edit
        edit?
        edit-start-byte
        edit-delete-byte-length
        edit-inserted-text
        apply-edit
        parse-source/incremental
        make-incremental-session
        incremental-session?
        incremental-session-artifact
        parse-incremental-session)

(def +edit-schema+ "gerbil-parser.edit.v1")
(def +incremental-receipt-schema+
  "gerbil-parser.incremental-receipt.v1")
(def +checkpoint-spacing+ 32)

(defstruct incremental-session-state
  (machine source artifact tokens modes checkpoints)
  transparent: #t)
(def incremental-session? incremental-session-state?)
(def incremental-session-artifact incremental-session-state-artifact)

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

;; : (-> Nat Nat String Edit)
(def (make-edit start-byte delete-byte-length inserted-text)
  (unless (and (integer? start-byte) (>= start-byte 0)
               (integer? delete-byte-length) (>= delete-byte-length 0)
               (string? inserted-text))
    (error "invalid incremental edit" start-byte delete-byte-length
           inserted-text))
  (make-edit-record +edit-schema+ start-byte delete-byte-length
                    inserted-text))

;; : (-> String Nat Nat)
(def (byte-index->character-index source-bytes target)
  (let (source-byte-length (u8vector-length source-bytes))
    (unless (<= 0 target source-byte-length)
      (error "edit byte offset is outside source" target source-byte-length))
    (when (and (< target source-byte-length)
               (<= 128 (u8vector-ref source-bytes target) 191))
      (error "edit byte offset splits a UTF-8 character" target))
    (let loop ((byte 0) (character 0))
      (if (= byte target)
        character
        (loop (+ byte 1)
              (if (<= 128 (u8vector-ref source-bytes byte) 191)
                character
                (+ character 1)))))))

;; : (-> String Edit String)
(def (apply-edit source source-edit)
  (unless (and (string? source)
               (edit? source-edit)
               (equal? (edit-record-schema source-edit) +edit-schema+))
    (error "apply-edit requires source and Edit v1"))
  (let* ((start-byte (edit-start-byte source-edit))
         (end-byte (+ start-byte (edit-delete-byte-length source-edit)))
         (source-bytes (string->utf8 source))
         (start (byte-index->character-index source-bytes start-byte))
         (end (byte-index->character-index source-bytes end-byte)))
    (string-append (substring source 0 start)
                   (edit-inserted-text source-edit)
                   (substring source end (string-length source)))))

;; : (-> ParseArtifact (List Token))
(def (artifact-tokens artifact)
  (filter-map
   (lambda (event)
     (and (token-event? event)
          (make-token (token-event-token-kind event)
                      (token-event-lexeme event)
                      (event-start event) (event-end event))))
   (parse-artifact-events artifact)))

;; Only the closed lexical algebra has position-local scanners. An external
;; scanner may consult the source prefix, so a matching byte boundary alone
;; cannot prove that its later tokens are unchanged.
(def (closed-lexical-expression? expression)
  (case (car expression)
    ((external) #f)
    ((choice) (every closed-lexical-expression? (cdr expression)))
    ((precedence) (closed-lexical-expression? (caddr expression)))
    (else #t)))

(def (closed-lexer? machine)
  (every (lambda (row) (closed-lexical-expression? (cadr row)))
         (parser-ir-ref (parser-machine-ir machine) 'lexical-rules)))

(def (relocate-token source-token byte-delta)
  (make-token (token-kind source-token) (token-lexeme source-token)
              (+ (token-start source-token) byte-delta)
              (+ (token-end source-token) byte-delta)))

(def (incremental-receipt fields artifact (checkpoint-shifts #f))
  (append
   fields
   (if checkpoint-shifts
     (list (cons 'checkpointReusedShiftCount checkpoint-shifts))
     '())
   (list (cons 'publicationSchema
               (parse-artifact-ref artifact 'schema)))))

(def (require-incremental-base machine old-source old-artifact)
  (unless (and (parse-artifact-valid? old-artifact)
               (equal? (parse-artifact-ref old-artifact 'sourceDigest)
                       (sha256-text old-source))
               (equal? (parse-artifact-ref old-artifact 'grammarDigest)
                       (parser-machine-grammar-digest machine)))
    (error "incremental base artifact does not match source and grammar")))

(def (parse-source/incremental machine old-source old-artifact source-edit)
  (require-incremental-base machine old-source old-artifact)
  (let-values (((next receipt)
                (parse-incremental-session
                 (session-from-directed machine old-source) source-edit)))
    (values (incremental-session-artifact next) receipt)))

(def (session-from-directed machine source)
  (let-values (((artifact source-tokens source-modes checkpoints)
                (parse-source/checkpoints
                 machine source +checkpoint-spacing+)))
    (make-incremental-session-state
     machine source artifact
     (or source-tokens (artifact-tokens artifact))
     source-modes checkpoints)))

(def (make-incremental-session machine source)
  (session-from-directed machine source))

;; The checkpoint stores its exact source-token cursor, including trivia.
;; Return both sides so the edit driver never scans the accepted prefix again.
(def (split-checkpoint-prefix tokens modes count)
  (let loop ((remaining tokens) (remaining-modes modes)
             (left count) (prefix '()) (prefix-modes '()))
    (if (zero? left)
      (values (reverse prefix) (reverse prefix-modes)
              remaining remaining-modes)
      (if (or (null? remaining) (null? remaining-modes))
        (error "checkpoint exceeds source token stream" count)
        (loop (cdr remaining) (cdr remaining-modes) (- left 1)
              (cons (car remaining) prefix)
              (cons (car remaining-modes) prefix-modes))))))

;; Snapshot byte ends are monotone. The strict boundary matches the old
;; reusable-prefix contract: a token ending at the edit start is rescanned.
(def (checkpoint-before-byte checkpoints edit-start)
  (let loop ((index (- (vector-length checkpoints) 1)))
    (if (or (zero? index)
            (< (vector-ref (vector-ref checkpoints index) 3)
               edit-start))
      index
      (loop (- index 1)))))

(def (edit-byte-delta source-edit)
  (- (u8vector-length (string->utf8 (edit-inserted-text source-edit)))
     (edit-delete-byte-length source-edit)))

(def (directed-edit-fields machine old-source new-source source-edit
                           prefix-count shifted reused-tail-count
                           reused-tail-bytes restart-byte significant-count
                           (reused-significant-count 0)
                           (relex-stop-byte #f))
  (let* ((source-byte-length (u8vector-length (string->utf8 new-source)))
         (byte-delta (edit-byte-delta source-edit)))
    (list
     (cons 'schema +incremental-receipt-schema+)
     (cons 'grammarDigest (parser-machine-grammar-digest machine))
     (cons 'baseSourceDigest (sha256-text old-source))
     (cons 'sourceDigest (sha256-text new-source))
     (cons 'edit
           (list (cons 'schema +edit-schema+)
                 (cons 'startByte (edit-start-byte source-edit))
                 (cons 'deleteByteLength
                       (edit-delete-byte-length source-edit))
                 (cons 'insertedText (edit-inserted-text source-edit))))
     (cons 'relexStartByte restart-byte)
     (cons 'relexStopByte (or relex-stop-byte source-byte-length))
     (cons 'relexedByteCount
           (- source-byte-length restart-byte reused-tail-bytes))
     (cons 'reusedTokenIds (iota prefix-count))
     (cons 'reusedTokenCount prefix-count)
     (cons 'suffixByteDelta byte-delta)
     (cons 'convergedSuffixTokenCount reused-tail-count)
     (cons 'reusedSuffixTokenCount
           (if (zero? byte-delta) reused-tail-count 0))
     (cons 'relocatedSuffixTokenCount
           (if (zero? byte-delta) 0 reused-tail-count))
     (cons 'resumedSignificantTokenCount shifted)
     (cons 'reusedSignificantTokenCount reused-significant-count)
     (cons 'remainingSignificantTokenCount
           (- significant-count shifted reused-significant-count)))))

;;; A same-width edit confined to one trivia token cannot change the LR
;;; frontier when the closed, mode-directed scanner still emits that token kind
;;; over the same byte range. Significant tokens and sparse recognition
;;; checkpoints remain valid, so the event structure can be reused verbatim.
(def (same-width-trivia-reuse session source-edit new-source)
  (let* ((machine (incremental-session-state-machine session))
         (old-artifact (incremental-session-state-artifact session))
         (old-tokens (incremental-session-state-tokens session))
         (old-modes (incremental-session-state-modes session))
         (edit-start (edit-start-byte source-edit))
         (edit-end (+ edit-start (edit-delete-byte-length source-edit))))
    (and (zero? (edit-byte-delta source-edit))
         (parse-artifact-success? old-artifact)
         old-modes
         (closed-lexer? machine)
         (let loop ((tokens old-tokens) (modes old-modes) (index 0))
           (and (pair? tokens) (pair? modes)
                (let ((old-token (car tokens))
                      (start (token-start (car tokens)))
                      (end (token-end (car tokens))))
                  (cond
                   ((<= end edit-start)
                    (loop (cdr tokens) (cdr modes) (+ index 1)))
                   ((and (<= start edit-start) (<= edit-end end)
                         ((parser-machine-trivia machine) old-token))
                    (let (new-token
                          (with-catch
                           (lambda (_condition) #f)
                           (lambda ()
                             (let* ((mode
                                     (vector-ref
                                      (lr-runtime-lexical-mode-catalog
                                       (parser-machine-runtime machine))
                                      (car modes)))
                                    (character
                                     (byte-index->character-index
                                      (string->utf8 new-source) start)))
                               (let-values (((token _next-character)
                                             (scan-source-token
                                              machine new-source character
                                              start mode)))
                                 token)))))
                      (and new-token
                           (= (token-start new-token) start)
                           (= (token-end new-token) end)
                           (eq? (token-kind new-token)
                                (token-kind old-token))
                           ((parser-machine-trivia machine) new-token)
                           (let* ((artifact
                                   (make-same-width-trivia-artifact
                                    old-artifact new-source index new-token))
                                  (next-tokens
                                   (append (take old-tokens index)
                                           (cons new-token (cdr tokens))))
                                  (next
                                   (make-incremental-session-state
                                    machine new-source artifact next-tokens
                                    old-modes
                                    (incremental-session-state-checkpoints
                                     session))))
                             (vector next index (length (cdr tokens))
                                     (- (u8vector-length
                                         (string->utf8 new-source)) end)
                                     start
                                     (- (length (parse-artifact-events
                                                 old-artifact))
                                        1))))))
                   (else #f))))))))

(def (parse-incremental-session session source-edit)
  (unless (incremental-session? session)
    (error "incremental edit requires a session" session))
  (let* ((machine (incremental-session-state-machine session))
         (old-source (incremental-session-state-source session))
         (old-tokens (incremental-session-state-tokens session))
         (old-modes (incremental-session-state-modes session))
         (checkpoints (incremental-session-state-checkpoints session))
         (new-source (apply-edit old-source source-edit))
         (source-byte-length (u8vector-length (string->utf8 new-source))))
    (def (finish next prefix-count shifted reused-count reused-bytes restart-byte
                 fresh? (reused-events #f))
      (let* ((artifact (incremental-session-artifact next))
             (significant-count
              (length
               (parser-significant-tokens
                machine (incremental-session-state-tokens next))))
             (fields
              (directed-edit-fields
               machine old-source new-source source-edit
               prefix-count shifted reused-count reused-bytes
               restart-byte significant-count
               (if reused-events significant-count 0)
               (and reused-events (- source-byte-length reused-bytes)))))
        (values
         next
         (incremental-receipt
          (cond
           (fresh? (cons (cons 'freshFallback? #t) fields))
           (reused-events
            (cons (cons 'reusedRecognitionEventCount reused-events) fields))
           (else fields))
          artifact (and (not reused-events) shifted)))))
    (def (fallback)
      (finish (session-from-directed machine new-source)
              0 0 0 0 0 #t))
    (let (trivia-reuse
          (same-width-trivia-reuse session source-edit new-source))
    (if trivia-reuse
      (finish (vector-ref trivia-reuse 0)
              (vector-ref trivia-reuse 1) 0
              (vector-ref trivia-reuse 2)
              (vector-ref trivia-reuse 3)
              (vector-ref trivia-reuse 4) #f
              (vector-ref trivia-reuse 5))
    (if (or (zero? (vector-length checkpoints))
            (not old-modes)
            (not (closed-lexer? machine)))
      (fallback)
      (let* ((index
              (checkpoint-before-byte
               checkpoints (edit-start-byte source-edit)))
             (saved (vector-ref checkpoints index))
             (shifted (vector-ref saved 0)))
        (let-values (((prefix prefix-modes old-rest mode-rest)
                      (split-checkpoint-prefix
                       old-tokens old-modes (vector-ref saved 2))))
          (let* ((restart-byte (vector-ref saved 3))
                 (restart-character
                  (byte-index->character-index
                   (string->utf8 new-source) restart-byte))
                 (byte-delta (edit-byte-delta source-edit))
                 (edit-end
                  (+ (edit-start-byte source-edit)
                     (edit-delete-byte-length source-edit)))
                 (reused-count 0)
                 (reused-bytes 0)
                 (reuse-token
                  (lambda (character byte mode)
                    (when (null? mode-rest)
                      (set! old-rest '()))
                    (let skip ()
                      (when (and (pair? old-rest)
                                 (or (< (token-start (car old-rest)) edit-end)
                                     (< (+ (token-start (car old-rest))
                                           byte-delta)
                                        byte)))
                        (set! old-rest (cdr old-rest))
                        (when (pair? mode-rest)
                          (set! mode-rest (cdr mode-rest)))
                        (skip)))
                    (and (pair? old-rest)
                         (pair? mode-rest)
                         (= (+ (token-start (car old-rest)) byte-delta)
                            byte)
                         (= (car mode-rest) (lr-lexical-mode-id mode))
                         (let* ((old-token (car old-rest))
                                (new-token
                                 (if (zero? byte-delta)
                                   old-token
                                   (relocate-token old-token byte-delta))))
                           (set! old-rest (cdr old-rest))
                           (set! mode-rest (cdr mode-rest))
                           (set! reused-count (+ reused-count 1))
                           (set! reused-bytes
                                 (+ reused-bytes
                                    (- (token-end old-token)
                                       (token-start old-token))))
                           (cons new-token
                                 (+ character
                                    (string-length (token-lexeme old-token))))))))
                 (rebound
                  (lr-prefix-snapshot-rebind
                   (vector-ref saved 1) prefix '() restart-byte)))
            (let-values
                (((artifact tokens modes records)
                  (parse-source/checkpoints/resume
                   machine new-source +checkpoint-spacing+ rebound
                   prefix prefix-modes restart-character restart-byte
                   reuse-token)))
              (if (not (parse-artifact-success? artifact))
                (fallback)
                (let* ((next-checkpoints
                        (if (zero? (vector-length records))
                          #()
                          (list->vector
                           (append
                            (take (vector->list checkpoints) index)
                            (vector->list records)))))
                       (next
                        (make-incremental-session-state
                         machine new-source artifact tokens modes
                         next-checkpoints)))
                  (finish next (vector-ref saved 2) shifted
                          reused-count reused-bytes restart-byte #f))))))))))))
