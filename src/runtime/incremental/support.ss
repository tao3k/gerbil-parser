;;; -*- Gerbil -*-
;;; incremental: support owner. Private execution state and named algorithms.

(import (only-in :std/string/utf8
                 string-utf8-length)
        (only-in ../../compiler/machine
                 parser-machine-grammar-digest parser-machine-ir parser-machine-runtime)
        (only-in ../parser-ir-data
                 parser-ir-ref)
        (only-in ../artifact
                 event-end event-start parse-artifact-events parse-artifact-ref parse-artifact-valid? token-event? token-event-lexeme token-event-token-kind)
        (only-in ../identity
                 sha256-text)
        (only-in ../lr-parser
                 lr-lexical-mode-terminals lr-runtime-lexical-mode-catalog)
        (only-in ../lexer
                 scan-source-token)
        (only-in ../token
                 make-token token-end token-kind token-lexeme token-start)
        (only-in "./state.ss"
                 +edit-schema+ +incremental-receipt-schema+ make-edit-record edit-record-schema edit? edit-start-byte edit-delete-byte-length edit-inserted-text))
(export make-edit
        byte-index->character-index
        apply-edit
        artifact-tokens
        closed-lexical-expression?
        closed-lexer?
        relocate-token
        incremental-receipt
        require-incremental-base
        split-checkpoint-prefix
        checkpoint-before-byte
        edit-byte-delta
        directed-edit-fields
        generic-lr-lexemes?
        same-scanned-token?)

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
  (- (string-utf8-length (edit-inserted-text source-edit))
     (edit-delete-byte-length source-edit)))

(def (directed-edit-fields machine old-source new-source source-edit
                           prefix-count shifted reused-tail-count
                           reused-tail-bytes restart-byte significant-count
                           (reused-significant-count 0)
                           (relex-stop-byte #f))
  (let* ((source-byte-length (string-utf8-length new-source))
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

;;; Literal actions refine token kinds. A changed significant token can retain
;;; the LR action sequence only when neither lexeme matches any literal in the
;;; runtime's interned lexical modes (including case-insensitive lookup).
(def (generic-lr-lexemes? runtime old-lexeme new-lexeme)
  (let ((catalog (lr-runtime-lexical-mode-catalog runtime))
        (old-upper (string-upcase old-lexeme))
        (new-upper (string-upcase new-lexeme)))
    (let mode-loop ((index 0))
      (or (= index (vector-length catalog))
          (and (every
                (lambda (terminal)
                  (or (not (eq? (cadr terminal) 'literal))
                      (and (not (equal? (caddr terminal) old-lexeme))
                           (not (equal? (caddr terminal) old-upper))
                           (not (equal? (caddr terminal) new-lexeme))
                           (not (equal? (caddr terminal) new-upper)))))
                (lr-lexical-mode-terminals
                 (vector-ref catalog index)))
               (mode-loop (+ index 1)))))))

(def (same-scanned-token? machine source old-token mode-id delta)
  (with-catch
   (lambda (_condition) #f)
   (lambda ()
     (let* ((start (+ (token-start old-token) delta))
            (mode
             (vector-ref
              (lr-runtime-lexical-mode-catalog
               (parser-machine-runtime machine)) mode-id))
            (character
             (byte-index->character-index (string->utf8 source) start)))
       (let-values (((token _next-character)
                     (scan-source-token machine source character start mode)))
         (and (eq? (token-kind token) (token-kind old-token))
              (equal? (token-lexeme token) (token-lexeme old-token))
              (= (token-end token) (+ (token-end old-token) delta))))))))

