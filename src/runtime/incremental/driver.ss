;;; -*- Gerbil -*-
;;; incremental: driver owner. Private execution state and named algorithms.

(import (only-in :std/string/utf8
                 string-utf8-length)
        (only-in ../source-index
                 source-index-count source-index-height source-index-end source-index-slice make-source-index-builder source-index-builder-token! source-index-builder-shared! source-index-builder-finish source-index-builder-fresh source-index-builder-shared source-index-builder-chunks source-index-builder-mode-chunks make-source-index-cursor source-index-cursor-token source-index-cursor-mode source-index-cursor-rank source-index-cursor-next! source-index-cursor-seek!)
        (only-in ../machine
                 parser-machine-runtime)
        (only-in ../artifact
                 parse-artifact-ref parse-artifact-success?)
        (only-in ../reuse
                 make-fragment-reuser)
        (only-in ../lr-parser
                 lr-lexical-mode-id lr-runtime-fragment-reuse-safe? lr-prefix-snapshot-rebind)
        (only-in ../parser
                 current-source-stream-observer parse-source/checkpoints/resume)
        (only-in ../significant
                 parser-significant-tokens)
        (only-in ../token
                 token-end token-kind token-lexeme token-start)
        (only-in "./state.ss"
                 +checkpoint-spacing+ current-lr-fragment-reuse-enabled? current-lr-source-index-enabled? make-incremental-session-state incremental-session-state-machine incremental-session-state-source incremental-session-state-tokens incremental-session-state-modes incremental-session-state-checkpoints incremental-session-state-capture? incremental-session? incremental-session-artifact incremental-session-source-index incremental-session-recognition-root edit-start-byte edit-delete-byte-length)
        (only-in "./reuse.ss"
                 source-mode-compatible? certified-token-reuse certified-token-window-reuse)
        (only-in "./session.ss"
                 capture-session-parse session-from-directed)
        (only-in "./support.ss"
                 byte-index->character-index apply-edit closed-lexer? relocate-token incremental-receipt require-incremental-base split-checkpoint-prefix checkpoint-before-byte edit-byte-delta directed-edit-fields))
(export parse-source/incremental
        parse-incremental-session)

(def (parse-source/incremental machine old-source old-artifact source-edit)
  (require-incremental-base machine old-source old-artifact)
  (let-values (((next receipt)
                (parse-incremental-session
                 (session-from-directed machine old-source) source-edit)))
    (values (incremental-session-artifact next) receipt)))

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
         (source-byte-length (string-utf8-length new-source))
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
                (begin
                  (when (getenv "GERBIL_PARSER_LR_TRACE" #f)
                    (write (list 'CHECKPOINT-RESUME-REJECTED 'index index
                             'checkpoint-count (vector-length checkpoints)
                             'restart-byte restart-byte
                             'diagnostics (parse-artifact-ref artifact 'diagnostics)))
                    (newline) (force-output))
                  (fallback))
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

