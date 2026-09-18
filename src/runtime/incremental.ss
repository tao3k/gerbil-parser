;;; -*- Gerbil -*-
;;; Conservative prefix token reuse with fresh-parse-equivalent publication.

(import (only-in :std/srfi/1 drop filter filter-map take)
        (only-in ../compiler/machine
                 parser-machine-grammar-digest parser-machine-runtime)
        (only-in ./artifact
                 event-end event-start parse-artifact-events
                 parse-artifact-ref parse-artifact-valid? token-event?
                 token-event-lexeme token-event-token-kind)
        (only-in ./identity sha256-text)
        (only-in ./lexer lex-source-from)
        (only-in ./lr-parser
                 lr-checkpoint-advance-shifts lr-initial-checkpoint)
        (only-in ./parser parse-tokenized/checkpoint)
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
        parse-source/incremental)

(def +edit-schema+ "gerbil-parser.edit.v1")
(def +incremental-receipt-schema+
  "gerbil-parser.incremental-receipt.v1")

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
(def (byte-index->character-index source target)
  (let ((character-length (string-length source))
        (source-byte-length (u8vector-length (string->utf8 source))))
    (unless (<= 0 target source-byte-length)
      (error "edit byte offset is outside source" target source-byte-length))
    (let loop ((character 0) (byte 0))
      (cond
       ((= byte target) character)
       ((= character character-length)
        (error "edit byte offset splits a UTF-8 character" target))
       (else
        (let (next
              (+ byte
                 (u8vector-length
                  (string->utf8 (string (string-ref source character))))))
          (when (> next target)
            (error "edit byte offset splits a UTF-8 character" target))
          (loop (+ character 1) next)))))))

;; : (-> String Edit String)
(def (apply-edit source source-edit)
  (unless (and (string? source)
               (edit? source-edit)
               (equal? (edit-record-schema source-edit) +edit-schema+))
    (error "apply-edit requires source and Edit v1"))
  (let* ((start-byte (edit-start-byte source-edit))
         (end-byte (+ start-byte (edit-delete-byte-length source-edit)))
         (start (byte-index->character-index source start-byte))
         (end (byte-index->character-index source end-byte)))
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

;; : (-> (List Token) Nat (List Token))
(def (reusable-prefix tokens edit-start)
  (filter (lambda (source-token)
            (< (token-end source-token) edit-start))
          tokens))

;; : (-> Token Token Integer Boolean)
(def (shift-equivalent-token? old-token new-token byte-delta)
  (and (eq? (token-kind old-token) (token-kind new-token))
       (equal? (token-lexeme old-token) (token-lexeme new-token))
       (= (+ (token-start old-token) byte-delta)
          (token-start new-token))
       (= (+ (token-end old-token) byte-delta)
          (token-end new-token))))

;;; Computes maximal suffix convergence in one reverse vector walk. Absolute
;;; token objects are reusable only when the edit has zero net byte delta.
;; : (-> (List Token) (List Token) Integer Nat)
(def (converged-suffix-count old-suffix new-suffix byte-delta)
  (let* ((old (list->vector old-suffix))
         (new (list->vector new-suffix)))
    (let loop ((old-index (- (vector-length old) 1))
               (new-index (- (vector-length new) 1))
               (count 0))
      (if (and (>= old-index 0)
               (>= new-index 0)
               (shift-equivalent-token?
                (vector-ref old old-index)
                (vector-ref new new-index)
                byte-delta))
        (loop (- old-index 1) (- new-index 1) (+ count 1))
        count))))

;;; Reuses the maximal prefix that cannot touch the edit, re-lexes from its
;;; terminal boundary, and submits the complete token stream to the ordinary
;;; parser/ParseArtifact v1 commit path.
;; parse-source/incremental
;;   : (-> ParserMachine String ParseArtifact Edit
;;          (Values ParseArtifact IncrementalReceipt))
;;   | doc m%
;;       Reuses a safe token prefix and emits an ordinary ParseArtifact v1.
;;
;;       # Examples
;;
;;       ```scheme
;;       (parse-source/incremental machine old-source artifact edit)
;;       ;; => fresh-equivalent artifact plus reuse receipt
;;       ```
;;     %
(def (parse-source/incremental machine old-source old-artifact source-edit)
  (unless (and (parse-artifact-valid? old-artifact)
               (equal? (parse-artifact-ref old-artifact 'sourceDigest)
                       (sha256-text old-source))
               (equal? (parse-artifact-ref old-artifact 'grammarDigest)
                       (parser-machine-grammar-digest machine)))
    (error "incremental base artifact does not match source and grammar"))
  (let* ((new-source (apply-edit old-source source-edit))
         (old-tokens (artifact-tokens old-artifact))
         (prefix (reusable-prefix old-tokens (edit-start-byte source-edit)))
         (old-suffix (drop old-tokens (length prefix)))
         (restart-byte (if (pair? prefix) (token-end (last prefix)) 0))
         (restart-character
          (byte-index->character-index new-source restart-byte))
         (suffix
          (lex-source-from machine new-source restart-character restart-byte))
         (byte-delta
          (- (u8vector-length (string->utf8 new-source))
             (u8vector-length (string->utf8 old-source))))
         (converged-count
          (converged-suffix-count old-suffix suffix byte-delta))
         (changed-suffix-count (- (length suffix) converged-count))
         (reused-suffix-count (if (zero? byte-delta) converged-count 0))
         (relocated-suffix-count
          (if (zero? byte-delta) 0 converged-count))
         (converged-tail
          (if (zero? byte-delta)
            (drop old-suffix (- (length old-suffix) converged-count))
            (drop suffix changed-suffix-count)))
         (tokens
          (append prefix
                  (take suffix changed-suffix-count)
                  converged-tail))
         (significant (parser-significant-tokens machine tokens))
         (reused-significant-count
          (length (parser-significant-tokens machine prefix))))
    (let-values
        (((checkpoint-status checkpoint-payload)
          (if (positive? reused-significant-count)
            (lr-checkpoint-advance-shifts
             (lr-initial-checkpoint
              (parser-machine-runtime machine) significant)
             reused-significant-count)
            (values
             'checkpoint
             (lr-initial-checkpoint
              (parser-machine-runtime machine) significant)))))
      (unless (eq? checkpoint-status 'checkpoint)
        (error "incremental prefix did not end at an LR checkpoint"
               checkpoint-status))
      (let* ((artifact
              (parse-tokenized/checkpoint
               machine (parser-machine-grammar-digest machine)
               new-source tokens checkpoint-payload))
         (receipt
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
                       (cons 'insertedText
                             (edit-inserted-text source-edit))))
           (cons 'relexStartByte restart-byte)
           (cons 'reusedTokenIds (iota (length prefix)))
           (cons 'reusedTokenCount (length prefix))
           (cons 'suffixByteDelta byte-delta)
           (cons 'convergedSuffixTokenCount converged-count)
           (cons 'reusedSuffixTokenCount reused-suffix-count)
           (cons 'relocatedSuffixTokenCount relocated-suffix-count)
           (cons 'resumedSignificantTokenCount reused-significant-count)
           (cons 'remainingSignificantTokenCount
                 (- (length significant) reused-significant-count))
           (cons 'publicationSchema
                 (parse-artifact-ref artifact 'schema)))))
        (values artifact receipt)))))
