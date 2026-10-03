;;; -*- Gerbil -*-
;;; LR checkpoint resume with lexical-mode-certified token reuse.

(import (only-in ./probe current-lr-lexical-plan-reuse-enabled?)
        (only-in ./source-index
                 source-index-count source-index-height source-index-end source-index-slice
                 make-source-index-builder source-index-builder-token! source-index-builder-shared!
                 source-index-builder-finish source-index-builder-fresh source-index-builder-shared
                 source-index-builder-chunks source-index-builder-mode-chunks make-source-index-cursor source-index-cursor-token
                 source-index-cursor-mode source-index-cursor-rank source-index-cursor-next!
                 source-index-cursor-seek!)
        (only-in ../compiler/machine
                 parser-machine-grammar-digest parser-machine-ir
                 parser-machine-runtime parser-machine-trivia parser-machine-lexical-modes-compatible?
                 parser-machine-for-current-semantic-backend)
        (only-in ../compiler/parser-ir parser-ir-ref)
        (only-in ./artifact
                 event-end event-start make-success-parse-artifact make-same-width-token-artifact
                 make-shifted-token-artifact
                 make-certified-window-artifact
                 parse-artifact-events
                 parse-artifact-ref parse-artifact-success?
                 parse-artifact-valid? token-event?
                 token-event-lexeme token-event-token-kind)
        (only-in ./recognition recognition-child-value relocate-recognition-value)
        (only-in ./event-program event-program-value? event-program-relocate)
        (only-in ./funcs recognition-sequence->list)
        (only-in ./reuse make-fragment-reuser current-lr-probe-reuse-enabled?)
        (only-in ./identity sha256-text)
        (only-in ./lr-parser
                 lr-lexical-mode-id lr-lexical-mode-terminals
                 lr-runtime-lexical-mode-catalog
                 lr-runtime-fragment-reuse-safe?
                 lr-prefix-snapshot-rebind current-lr-recognition-observer lr-recognition-project
                 lr-recognition-view? lr-recognition-view-base lr-recognition-view-delta
                 lr-recognition-fragment-value)
        (only-in ./lexer scan-source-token)
        (only-in ./parser
                 current-source-stream-observer parse-source/checkpoints
                 parse-source/checkpoints/resume)
        (only-in ./significant parser-significant-tokens)
        (only-in ./token
                 make-token token-end token-kind token-lexeme token-start))
(export current-lr-lexical-plan-reuse-enabled? current-lr-source-index-enabled? incremental-session-source-index incremental-session-source-modes
        current-lr-probe-reuse-enabled? current-lr-fragment-reuse-enabled?
        +edit-schema+
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
        incremental-session-recognition-root
        incremental-session-project-artifact
        incremental-session-publication-comparison
        parse-incremental-session)

(def +edit-schema+ "gerbil-parser.edit.v1")
(def +incremental-receipt-schema+
  "gerbil-parser.incremental-receipt.v1")
(def +checkpoint-spacing+ 32)
;;; Experimental capture sessions can compare the same executor with transfers
;;; enabled/disabled. Ordinary sessions still do not capture a grammar forest.
(def current-lr-fragment-reuse-enabled? (make-parameter #t))

;;; Compiler candidate-set certificates affect lexical reuse only.
(def (source-mode-compatible? machine old-mode mode old-token)
  (let (new-mode (lr-lexical-mode-id mode))
    (or (= old-mode new-mode)
        (and (current-lr-lexical-plan-reuse-enabled?)
             (parser-machine-lexical-modes-compatible? machine old-mode new-mode
               (string-ref (token-lexeme old-token) 0))))))
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

(def (incremental-session-project-artifact session)
  (let* ((root (incremental-session-recognition-root session))
         (machine (incremental-session-state-machine session)))
    (unless root (error "session has no certified grammar snapshot"))
    (let (children (recognition-sequence->list (lr-recognition-project root
                                 (incremental-session-state-tokens session))))
      (unless (and (pair? children) (null? (cdr children)))
        (error "grammar snapshot projection does not have one root"))
      ;; Publication consumes the exact captured source token instances, whose
      ;; identity is checked by the artifact owner against recognition leaves.
      (make-success-parse-artifact
       (parser-machine-grammar-digest machine)
       (incremental-session-state-source session)
       (incremental-session-state-tokens session)
       (recognition-child-value (car children)) (parser-machine-trivia machine)))))

;;; Internal diagnostic: projection is outside timing; publication thunks retain
;;; the session's exact machine, source and token identities. No token vector or
;;; mutable session execution field is exposed. Generic publication proofs stay.
(def (incremental-session-publication-comparison session)
  (unless (incremental-session? session) (error "publication requires a session"))
  (let* ((root (incremental-session-recognition-root session))
         (machine (incremental-session-state-machine session))
         (source (incremental-session-state-source session))
         (tokens (incremental-session-state-tokens session)))
    (unless root (error "publication requires a captured grammar root"))
    (def (single-value sequence)
      (let (children (recognition-sequence->list sequence))
        (unless (and (pair? children) (null? (cdr children)))
          (error "publication requires one semantic root"))
        (recognition-child-value (car children))))
    (let unwrap ((piece root) (delta 0) (moved? #f))
      (if (lr-recognition-view? piece)
        (unwrap (lr-recognition-view-base piece)
          (+ delta (lr-recognition-view-delta piece)) #t)
        (let* ((value (single-value (lr-recognition-fragment-value piece)))
               (canonical (single-value (lr-recognition-project root tokens)))
               (publish (lambda (value)
                          (make-success-parse-artifact (parser-machine-grammar-digest machine)
                            source tokens value (parser-machine-trivia machine)))))
          (unless (event-program-value? value)
            (error "publication comparison requires the event backend"))
          (let (program (if moved? (relocate-recognition-value value delta #t) value))
            (values (lambda () (publish canonical)) (lambda () (publish program))
                    (if moved? (event-program-relocate value delta #t) value))))))))

(def (capture-session-parse thunk capture?)
  (if (not capture?)
    (let-values (((artifact tokens modes checkpoints) (thunk)))
      (values artifact tokens modes checkpoints #f))
  (let ((root #f) (observer (current-lr-recognition-observer)))
    (parameterize
        ((current-lr-recognition-observer
          (lambda (value)
            (set! root value)
            (when observer (observer value)))))
      (let-values (((artifact tokens modes checkpoints) (thunk)))
        (values artifact tokens modes checkpoints
                (and (parse-artifact-success? artifact) root)))))))

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

(def (session-from-directed machine source (capture? #f))
  (let (builder (and capture? (current-lr-source-index-enabled?) (make-source-index-builder)))
    (let-values (((artifact source-tokens source-modes checkpoints root)
                  (parameterize
                      ((current-source-stream-observer
                        (and builder (lambda (token mode fragment?)
                          (when fragment? (error "fresh source driver transferred a fragment"))
                          (source-index-builder-token! builder token mode)))))
                    (capture-session-parse
                     (lambda () (parse-source/checkpoints machine source +checkpoint-spacing+)) capture?))))
      (make-incremental-session-state
       machine source artifact (or source-tokens (artifact-tokens artifact))
       source-modes checkpoints capture? root
       (and builder root source-modes (parse-artifact-success? artifact)
            (source-index-builder-finish builder))))))

;;; Certified event-window edits have an exact source-token splice. Preserve
;;; outside chunks even when the fast artifact path drops its grammar forest.
(def (splice-session-index session start deleted tokens modes delta)
  (let (old (and (current-lr-source-index-enabled?) (incremental-session-source-index session)))
    (and old
         (let (builder (make-source-index-builder (source-index-slice old 0 start)))
           (let loop ((tokens tokens) (modes modes))
             (unless (null? tokens)
               (unless (pair? modes) (error "source splice has missing lexical modes"))
               (source-index-builder-token! builder (car tokens) (car modes))
               (loop (cdr tokens) (cdr modes))))
           (source-index-builder-shared! builder old (+ start deleted)
              (- (source-index-count old) start deleted) delta)
           (source-index-builder-finish builder)))))

(def (make-incremental-session machine source (capture? #f))
  (unless (boolean? capture?) (error "invalid recognition capture option" capture?))
  (session-from-directed
   (if capture? (parser-machine-for-current-semantic-backend machine) machine)
   source capture?))

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

;;; A closed scanner and one-token edit certify unchanged suffix tokenization
;;; modulo its byte delta. Generic significant lexemes preserve LR actions.
;;; Snapshots after a shifted or significant token hold stale semantic values.
(def (certified-token-reuse session source-edit new-source)
  (let* ((machine (incremental-session-state-machine session))
         (old-artifact (incremental-session-state-artifact session))
         (old-tokens (incremental-session-state-tokens session))
         (old-modes (incremental-session-state-modes session))
         (edit-start (edit-start-byte source-edit))
         (edit-end (+ edit-start (edit-delete-byte-length source-edit)))
         (delta (edit-byte-delta source-edit)))
    (and (parse-artifact-success? old-artifact)
         old-modes
         (closed-lexer? machine)
         (let loop ((tokens old-tokens) (modes old-modes) (index 0)
                    (previous #f) (previous-mode #f))
           (and (pair? tokens) (pair? modes)
                (let ((old-token (car tokens))
                      (start (token-start (car tokens)))
                      (end (token-end (car tokens))))
                  (cond
                   ((<= end edit-start)
                    (loop (cdr tokens) (cdr modes) (+ index 1)
                          old-token (car modes)))
                   ((and (<= start edit-start) (<= edit-end end))
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
                           (= (token-end new-token) (+ end delta))
                           (eq? (token-kind new-token)
                                (token-kind old-token))
                           (eq? (not ((parser-machine-trivia machine)
                                      old-token))
                                (not ((parser-machine-trivia machine)
                                      new-token)))
                           (or ((parser-machine-trivia machine) new-token)
                               (generic-lr-lexemes?
                                (parser-machine-runtime machine)
                                (token-lexeme old-token)
                                (token-lexeme new-token)))
                           (or (zero? delta)
                               (and
                                (or (not previous)
                                    (same-scanned-token?
                                     machine new-source previous
                                     previous-mode 0))
                                (or (null? (cdr tokens))
                                    (same-scanned-token?
                                     machine new-source (cadr tokens)
                                     (cadr modes) delta))))
                           (let* ((artifact+shared
                                   (if (zero? delta)
                                     (cons
                                      (make-same-width-token-artifact
                                       old-artifact new-source index new-token)
                                      (- (length (parse-artifact-events
                                                  old-artifact)) 1))
                                     (let-values (((shifted shared)
                                                   (make-shifted-token-artifact
                                                    old-artifact new-source
                                                    index start end new-token
                                                    delta)))
                                       (cons shifted shared))))
                                  (artifact (car artifact+shared))
                                  (next-tokens
                                   (append (take old-tokens index)
                                           (cons new-token
                                                 (if (zero? delta)
                                                   (cdr tokens)
                                                   (map
                                                    (lambda (token)
                                                      (relocate-token token
                                                                      delta))
                                                    (cdr tokens))))))
                                  (trivia?
                                   ((parser-machine-trivia machine) new-token))
                                  (next-checkpoints
                                   (if (and trivia? (zero? delta))
                                     (incremental-session-state-checkpoints
                                      session)
                                     (list->vector
                                      (filter
                                       (lambda (record)
                                         (<= (vector-ref record 3) start))
                                       (vector->list
                                        (incremental-session-state-checkpoints
                                         session))))))
                                  (next
                                   (make-incremental-session-state
                                    machine new-source artifact next-tokens
                                    old-modes next-checkpoints
                                    (incremental-session-state-capture? session) #f
                                    (splice-session-index session index 1 (list new-token) (list (car modes)) delta))))
                             (vector next index (length (cdr tokens))
                                     (- (u8vector-length
                                         (string->utf8 new-source))
                                        (token-end new-token))
                                     start
                                     (cdr artifact+shared)
                                     (if trivia? 0 1))))))
                   (else #f))))))))

;;; Re-lex exactly the old token window in its recorded LR modes. The same
;;; number of tokens, kinds, trivia membership, and LR actions certify that
;;; recognition events have the same topology after a multi-token edit.
(def (scan-certified-window machine source old-window mode-ids start target-end)
  (with-catch
   (lambda (_condition) #f)
   (lambda ()
     (let loop ((old old-window) (modes mode-ids)
                (byte start)
                (character
                 (byte-index->character-index (string->utf8 source) start))
                (found '()) (changed-significant 0))
       (if (null? old)
         (and (= byte target-end)
              (cons (reverse found) changed-significant))
         (and (pair? modes)
              (let (mode
                    (vector-ref
                     (lr-runtime-lexical-mode-catalog
                      (parser-machine-runtime machine))
                     (car modes)))
                (let-values (((new-token next-character)
                              (scan-source-token
                               machine source character byte mode)))
                  (and (eq? (token-kind (car old))
                            (token-kind new-token))
                       (eq? (not ((parser-machine-trivia machine)
                                  (car old)))
                            (not ((parser-machine-trivia machine)
                                  new-token)))
                       (or ((parser-machine-trivia machine) new-token)
                           (equal? (token-lexeme (car old))
                                   (token-lexeme new-token))
                           (generic-lr-lexemes?
                            (parser-machine-runtime machine)
                            (token-lexeme (car old))
                            (token-lexeme new-token)))
                       (<= (token-end new-token) target-end)
                       (loop (cdr old) (cdr modes)
                             (token-end new-token) next-character
                             (cons new-token found)
                             (+ changed-significant
                                (if ((parser-machine-trivia machine)
                                     new-token)
                                  0 1))))))))))))

;;; Trivia leaves the LR checkpoint unchanged. A replacement can therefore
;;; contain a different number of trivia tokens when all old modes agree.
(def (scan-certified-trivia-window machine source mode-id start target-end)
  (with-catch
   (lambda (_condition) #f)
   (lambda ()
     (let (mode
           (vector-ref
            (lr-runtime-lexical-mode-catalog
             (parser-machine-runtime machine)) mode-id))
       (let loop ((byte start)
                  (character
                   (byte-index->character-index (string->utf8 source) start))
                  (found '()))
         (cond
          ((= byte target-end) (reverse found))
          ((> byte target-end) #f)
          (else
           (let-values (((token next-character)
                         (scan-source-token machine source character byte mode)))
             (and ((parser-machine-trivia machine) token)
                  (> (token-end token) byte)
                  (<= (token-end token) target-end)
                  (loop (token-end token) next-character
                        (cons token found)))))))))))

;;; Align significant tokens while allowing each existing trivia run to grow
;;; or shrink. Every run keeps its LR lexical mode; only significant tokens
;;; advance LR state. Boundaries inside a changed trivia run remain unmapped
;;; and cause event reuse to fail closed.
(def (scan-aligned-window machine source old-window mode-ids start target-end)
  (with-catch
   (lambda (_condition) #f)
   (lambda ()
     (let ((catalog
            (lr-runtime-lexical-mode-catalog
             (parser-machine-runtime machine)))
           (trivia? (parser-machine-trivia machine)))
       (def (scan byte character mode-id)
         (scan-source-token
          machine source character byte (vector-ref catalog mode-id)))
       (let loop ((old old-window) (modes mode-ids)
                  (byte start)
                  (character
                   (byte-index->character-index (string->utf8 source) start))
                  (tokens-rev '()) (modes-rev '()) (groups-rev '())
                  (boundaries '()) (significant 0))
         (if (null? old)
           (and (= byte target-end)
                (positive? significant)
                (vector (reverse tokens-rev) (reverse modes-rev)
                        (reverse groups-rev) boundaries significant))
           (and (pair? modes)
                (if (trivia? (car old))
                  (let gather ((run old) (run-modes modes)
                               (count 0) (last #f))
                    (if (and (pair? run) (trivia? (car run)))
                      (and (pair? run-modes)
                           (= (car run-modes) (car modes))
                           (gather (cdr run) (cdr run-modes)
                                   (+ count 1) (car run)))
                      (and (or (null? run)
                               (and (pair? run-modes)
                                    (= (car run-modes) (car modes))))
                           (let ()
                             (def (resume at at-character found)
                               (let (replacement (reverse found))
                                 (loop
                                  run run-modes at at-character
                                  (append found tokens-rev)
                                  (append (make-list (length found)
                                                     (car modes))
                                          modes-rev)
                                  (append (make-list (- count 1) '())
                                          (cons replacement groups-rev))
                                  (cons (cons (token-end last) at)
                                        (cons (cons (token-start (car old)) byte)
                                              boundaries))
                                  significant)))
                             (let collect ((at byte) (at-character character)
                                           (found '()))
                               (if (= at target-end)
                                 (resume at at-character found)
                                 (and (< at target-end)
                                      (let-values (((token next-character)
                                                    (scan at at-character
                                                          (car modes))))
                                        (if (trivia? token)
                                          (and (> (token-end token) at)
                                               (<= (token-end token) target-end)
                                               (collect (token-end token)
                                                        next-character
                                                        (cons token found)))
                                          (resume at at-character found))))))))))
                  (let-values (((new-token next-character)
                                (scan byte character (car modes))))
                    (and (not (trivia? new-token))
                         (eq? (token-kind (car old))
                              (token-kind new-token))
                         (or (equal? (token-lexeme (car old))
                                     (token-lexeme new-token))
                             (generic-lr-lexemes?
                              (parser-machine-runtime machine)
                              (token-lexeme (car old))
                              (token-lexeme new-token)))
                         (<= (token-end new-token) target-end)
                         (loop
                          (cdr old) (cdr modes)
                          (token-end new-token) next-character
                          (cons new-token tokens-rev)
                          (cons (car modes) modes-rev)
                          (cons (list new-token) groups-rev)
                          (cons (cons (token-end (car old))
                                      (token-end new-token))
                                (cons (cons (token-start (car old))
                                            (token-start new-token))
                                      boundaries))
                          (+ significant 1))))))))))))

(def (certified-token-window-reuse session source-edit new-source)
  (let* ((machine (incremental-session-state-machine session))
         (old-artifact (incremental-session-state-artifact session))
         (old-tokens (incremental-session-state-tokens session))
         (old-modes (incremental-session-state-modes session))
         (edit-start (edit-start-byte source-edit))
         (edit-end (+ edit-start (edit-delete-byte-length source-edit)))
         (delta (edit-byte-delta source-edit)))
    (and (parse-artifact-success? old-artifact)
         old-modes
         (closed-lexer? machine)
         (let locate ((tokens old-tokens) (modes old-modes)
                      (prefix '()) (prefix-modes '()) (index 0))
           (and (pair? tokens) (pair? modes)
                (if (<= (token-end (car tokens)) edit-start)
                  (locate (cdr tokens) (cdr modes)
                          (cons (car tokens) prefix)
                          (cons (car modes) prefix-modes) (+ index 1))
                  (let collect ((rest tokens) (rest-modes modes)
                                (window '()) (window-modes '())
                                (count 0))
                    (if (and (pair? rest)
                             (or (zero? count)
                                 (< (token-start (car rest)) edit-end)))
                      (and (pair? rest-modes)
                           (collect (cdr rest) (cdr rest-modes)
                                    (cons (car rest) window)
                                    (cons (car rest-modes) window-modes)
                                    (+ count 1)))
                      (and (positive? count)
                           (let* ((old-window (reverse window))
                                  (start (token-start (car old-window)))
                                  (target-end
                                   (+ (token-end (car window)) delta))
                                  (trivia-window?
                                   (and (every
                                         (parser-machine-trivia machine)
                                         old-window)
                                        (every
                                         (lambda (mode-id)
                                           (= mode-id (car window-modes)))
                                         window-modes)))
                                  (scanned
                                   (and (>= count 2)
                                        (scan-certified-window
                                         machine new-source old-window
                                         (reverse window-modes)
                                         start target-end)))
                                  (trivia-scanned
                                   (and (not scanned) trivia-window?
                                        (scan-certified-trivia-window
                                         machine new-source
                                         (car window-modes)
                                         start target-end)))
                                  (aligned
                                   (and (not scanned) (not trivia-scanned)
                                        (scan-aligned-window
                                         machine new-source old-window
                                         (reverse window-modes)
                                         start target-end)))
                                  (new-window
                                   (cond (scanned (car scanned))
                                         (trivia-scanned trivia-scanned)
                                         (aligned (vector-ref aligned 0))
                                         (else #f))))
                             (and new-window
                                  (or (null? prefix)
                                      (same-scanned-token?
                                       machine new-source (car prefix)
                                       (car prefix-modes) 0))
                                  (or (null? rest)
                                      (and (pair? rest-modes)
                                           (same-scanned-token?
                                            machine new-source (car rest)
                                            (car rest-modes) delta)))
                                  (let* ((next-tokens
                                          (append
                                           (reverse prefix) new-window
                                           (if (zero? delta) rest
                                               (map
                                                (lambda (token)
                                                  (relocate-token token delta))
                                                rest))))
                                         (next-modes
                                          (if scanned old-modes
                                              (append
                                               (reverse prefix-modes)
                                               (if aligned
                                                 (vector-ref aligned 1)
                                                 (make-list
                                                  (length new-window)
                                                  (car window-modes)))
                                               rest-modes)))
                                         (next-checkpoints
                                          (list->vector
                                           (filter
                                            (lambda (record)
                                              (<= (vector-ref record 3) start))
                                            (vector->list
                                             (incremental-session-state-checkpoints
                                              session)))))
                                         (suffix-bytes
                                          (- (u8vector-length
                                              (string->utf8 new-source))
                                             target-end)))
                                    (with-catch
                                     (lambda (_condition) #f)
                                     (lambda ()
                                       (let-values (((artifact shared)
                                                     (make-certified-window-artifact
                                                      old-artifact new-source
                                                      index old-window new-window
                                                      delta
                                                      (and aligned
                                                           (vector-ref aligned 2))
                                                      (and aligned
                                                           (vector-ref aligned 3)))))
                                         (vector
                                          (make-incremental-session-state
                                           machine new-source artifact next-tokens
                                           next-modes next-checkpoints
                                           (incremental-session-state-capture? session) #f
                                           (splice-session-index session index (length old-window) new-window
                                             (cond (scanned (reverse window-modes))
                                                   (aligned (vector-ref aligned 1))
                                                   (else (make-list (length new-window) (car window-modes)))) delta))
                                          index (length rest) suffix-bytes
                                          start shared
                                          (cond (scanned (cdr scanned))
                                                (aligned (vector-ref aligned 4))
                                                (else 0))))))))))))))))))

(def (parse-incremental-session session source-edit)
  (unless (incremental-session? session)
    (error "incremental edit requires a session" session))
  (let* ((machine (incremental-session-state-machine session))
         (old-source (incremental-session-state-source session))
         (old-tokens (incremental-session-state-tokens session))
         (old-modes (incremental-session-state-modes session))
         (checkpoints (incremental-session-state-checkpoints session))
         (old-index (and (current-lr-source-index-enabled?) (incremental-session-source-index session)))
         (index-builder #f) (index-origin #f) (provenance-cursor #f)
         (new-source (apply-edit old-source source-edit))
         (source-byte-length (u8vector-length (string->utf8 new-source)))
         (fragment-stats (make-vector 11 0)))
    (def (finish next prefix-count shifted reused-count reused-bytes restart-byte
                 fresh? (reused-events #f) (replaced-significant-count 0))
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
               (if reused-events
                 (- significant-count replaced-significant-count)
                 (vector-ref fragment-stats 1))
               (and reused-events (- source-byte-length reused-bytes)))))
        (values
         next
         (incremental-receipt
          (append
           (if (and index-builder (or (incremental-session-source-index next) (zero? source-byte-length)))
             (list (cons 'sourceIndexFreshTokenCount (source-index-builder-fresh index-builder))
                   (cons 'sourceIndexSharedTokenCount (source-index-builder-shared index-builder))
                   (cons 'sourceIndexNewChunkCount (source-index-builder-chunks index-builder))
                   (cons 'sourceIndexNewModeChunkCount (source-index-builder-mode-chunks index-builder))
                   (cons 'sourceIndexHeight (source-index-height (incremental-session-source-index next)))) '())
           (if (or (positive? (vector-ref fragment-stats 0))
                   (positive? (vector-ref fragment-stats 6))
                   (positive? (vector-ref fragment-stats 7))
                   (positive? (vector-ref fragment-stats 9)))
             (list (cons 'reusedRecognitionFragmentCount (vector-ref fragment-stats 0))
                   (cons 'fragmentCertificateProbeByteCount (vector-ref fragment-stats 4))
                   (cons 'fragmentRejectedProbeCount (vector-ref fragment-stats 5))
                   (cons 'fragmentControlProbeCount (vector-ref fragment-stats 6))
                   (cons 'fragmentProbeReuseTokenCount (vector-ref fragment-stats 7))
                   (cons 'fragmentProbeReuseByteCount (vector-ref fragment-stats 8))
                   (cons 'fragmentCursorVisitCount (vector-ref fragment-stats 9))
                   (cons 'certifiedLexicalProbeTokenCount (vector-ref fragment-stats 10))) '())
          (cond
           (fresh? (cons (cons 'freshFallback? #t) fields))
           (reused-events
            (cons (cons 'reusedRecognitionEventCount reused-events) fields))
           (else fields)))
          artifact (and (not reused-events) shifted)))))
    (def (fallback)
      (set! fragment-stats (make-vector 11 0))
      (set! index-builder #f) (set! index-origin #f)
      (finish (session-from-directed machine new-source
                                     (incremental-session-state-capture? session))
              0 0 0 0 0 #t))
    (let (token-reuse
          (certified-token-reuse session source-edit new-source))
    (if token-reuse
      (finish (vector-ref token-reuse 0)
              (vector-ref token-reuse 1) 0
              (vector-ref token-reuse 2)
              (vector-ref token-reuse 3)
              (vector-ref token-reuse 4) #f
              (vector-ref token-reuse 5)
              (vector-ref token-reuse 6))
    (let (window-reuse
          (certified-token-window-reuse session source-edit new-source))
    (if window-reuse
      (finish (vector-ref window-reuse 0)
              (vector-ref window-reuse 1) 0
              (vector-ref window-reuse 2)
              (vector-ref window-reuse 3)
              (vector-ref window-reuse 4) #f
              (vector-ref window-reuse 5)
              (vector-ref window-reuse 6))
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
                 (source-cursor (and old-index (make-source-index-cursor old-index (vector-ref saved 2))))
                 (reuse-token
                  (lambda (character byte mode)
                    (if source-cursor
                      (begin
                        (source-index-cursor-seek! source-cursor (max edit-end (- byte byte-delta)))
                        (let ((old-token (source-index-cursor-token source-cursor))
                              (old-mode (source-index-cursor-mode source-cursor)))
                          (and old-token (= (+ (token-start old-token) byte-delta) byte)
                               (source-mode-compatible? machine old-mode mode old-token)
                               (let ((rank (source-index-cursor-rank source-cursor))
                                     (token (if (zero? byte-delta) old-token (relocate-token old-token byte-delta))))
                                 (set! index-origin (vector rank 1 byte-delta
                                   (and (not (= old-mode (lr-lexical-mode-id mode))) (lr-lexical-mode-id mode))))
                                 (source-index-cursor-next! source-cursor)
                                 (set! reused-count (+ reused-count 1))
                                 (set! reused-bytes (+ reused-bytes (- (token-end old-token) (token-start old-token))))
                                 (cons token (+ character (string-length (token-lexeme old-token))))))))
                      (begin
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
                         (source-mode-compatible? machine (car mode-rest) mode (car old-rest))
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
                                    (string-length (token-lexeme old-token))))))))))
                 (rebound
                  (lr-prefix-snapshot-rebind
                   (vector-ref saved 1) prefix '() restart-byte)))
            (let-values (((reuse-fragment stats reuse-probe)
                          (if (and (incremental-session-state-capture? session)
                                   (incremental-session-recognition-root session)
                                   (current-lr-fragment-reuse-enabled?)
                                   ;; A short checkpoint tail costs less to replay than
                                   ;; to index/certify. Keep its existing LR path.
                                   (> (- (if old-index (source-index-count old-index) (length old-tokens)) (vector-ref saved 2))
                                      (* 2 +checkpoint-spacing+))
                                   (lr-runtime-fragment-reuse-safe? (parser-machine-runtime machine)))
                            (make-fragment-reuser
                             machine new-source old-tokens old-modes
                             (incremental-session-recognition-root session)
                             (edit-start-byte source-edit) edit-end byte-delta restart-byte
                             old-index (and old-index (lambda (start count delta)
                               (set! index-origin (vector start count delta #f))))
                             #t)
                            (values #f fragment-stats #f))))
              (set! fragment-stats stats)
              (when (and (incremental-session-state-capture? session) (current-lr-source-index-enabled?))
                (set! provenance-cursor (and old-index (make-source-index-cursor old-index (vector-ref saved 2))))
                (set! index-builder (make-source-index-builder
                  (and old-index (source-index-slice old-index 0 (vector-ref saved 2)))))
                ;; A newly enabled index has no old prefix tree: capture that
                ;; already-required prefix once, rather than invent provenance.
                (when (and (not old-index) (pair? prefix))
                  (let seed ((tokens prefix) (modes prefix-modes))
                    (unless (null? tokens)
                      (source-index-builder-token! index-builder (car tokens) (car modes))
                      (seed (cdr tokens) (cdr modes))))))
            (let-values
                (((artifact tokens modes records root)
                  (parameterize
                      ((current-source-stream-observer
                        (and index-builder
                          (lambda (piece modes fragment?)
                            ;; A scanner certificate may consume the boundary
                            ;; token before ordinary convergence runs. Recover
                            ;; source provenance only by exact token equality;
                            ;; a changed mode copies only bounded mode storage.
                            ;; Shared positions must be outside the changed bytes.
                            (when (and provenance-cursor (not fragment?) (not index-origin)
                                       (or (<= (token-end piece) (edit-start-byte source-edit))
                                           (>= (token-start piece) (+ edit-end byte-delta))))
                              (let (shift (if (<= (token-end piece) (edit-start-byte source-edit)) 0 byte-delta))
                                (source-index-cursor-seek! provenance-cursor (- (token-start piece) shift))
                                (let (old (source-index-cursor-token provenance-cursor))
                                  (when (and old
                                             (= (token-start piece) (+ shift (token-start old)))
                                             (= (token-end piece) (+ shift (token-end old)))
                                             (eq? (token-kind piece) (token-kind old))
                                             (equal? (token-lexeme piece) (token-lexeme old)))
                                    (set! index-origin (vector (source-index-cursor-rank provenance-cursor) 1 shift
                                      (and (not (= modes (source-index-cursor-mode provenance-cursor))) modes)))))))
                            (if index-origin
                              (begin
                                (when (and fragment? (not (= (length piece) (vector-ref index-origin 1))))
                                  (error "source provenance disagrees with fragment extent"))
                                (source-index-builder-shared! index-builder old-index
                                  (vector-ref index-origin 0) (vector-ref index-origin 1) (vector-ref index-origin 2) (vector-ref index-origin 3))
                                (set! index-origin #f))
                              (if fragment?
                                ;; The vector control has no persistent origin.
                                (let append ((tokens piece) (modes modes))
                                  (unless (null? tokens)
                                    (source-index-builder-token! index-builder (car tokens) (car modes))
                                    (append (cdr tokens) (cdr modes))))
                                (source-index-builder-token! index-builder piece modes)))))))
                    (capture-session-parse
                   (lambda ()
                     (parse-source/checkpoints/resume
                      machine new-source +checkpoint-spacing+ rebound
                      prefix prefix-modes restart-character restart-byte
                      (if reuse-probe
                        (lambda (character byte mode)
                          (or (reuse-probe character byte mode)
                              (reuse-token character byte mode)))
                        reuse-token)
                      reuse-fragment))
                   (incremental-session-state-capture? session)))))
              (if (not (parse-artifact-success? artifact))
                (fallback)
                (let* ((next-checkpoints
                        ;; Every checkpoint strictly before the selected one
                        ;; belongs to the unchanged prefix. A successful short
                        ;; tail may emit no new sample; retain that prefix so
                        ;; the next edit can resume rather than reparse in full.
                        (list->vector
                         (append
                          (take (vector->list checkpoints) index)
                          (vector->list records))))
                       (next
                        (make-incremental-session-state
                         machine new-source artifact tokens modes
                         next-checkpoints (incremental-session-state-capture? session) root
                         (and index-builder root modes
                              (let (tree (source-index-builder-finish index-builder))
                                (unless (or (and (not tree) (zero? source-byte-length))
                                            (= (source-index-end tree) source-byte-length))
                                  (error "source index does not cover successful source")) tree)))))
                  (finish next (vector-ref saved 2) shifted
                          (+ reused-count (vector-ref fragment-stats 2))
                          (+ reused-bytes (vector-ref fragment-stats 3)) restart-byte #f)))))))))))))))
