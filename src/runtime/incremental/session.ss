;;; -*- Gerbil -*-
;;; incremental: session owner. Private execution state and named algorithms.

(import (only-in ../source-index
                 source-index-count source-index-slice make-source-index-builder source-index-builder-token! source-index-builder-shared! source-index-builder-finish)
        (only-in ../machine
                 parser-machine-grammar-digest parser-machine-trivia parser-machine-for-current-semantic-backend)
        (only-in ../artifact
                 make-success-parse-artifact parse-artifact-success?)
        (only-in ../recognition
                 recognition-child-value relocate-recognition-value)
        (only-in ../event-program
                 event-program-value? event-program-relocate)
        (only-in ../funcs
                 recognition-sequence->list)
        (only-in ../lr-parser
                 current-lr-recognition-observer lr-recognition-project lr-recognition-view? lr-recognition-view-base lr-recognition-view-delta lr-recognition-fragment-value)
        (only-in ../parser
                 current-source-stream-observer parse-source/checkpoints)
        (only-in "./state.ss"
                 +checkpoint-spacing+ current-lr-source-index-enabled? make-incremental-session-state incremental-session-state-machine incremental-session-state-source incremental-session-state-tokens incremental-session? incremental-session-source-index incremental-session-recognition-root)
        (only-in "./support.ss"
                 artifact-tokens))
(export incremental-session-project-artifact
        incremental-session-publication-comparison
        capture-session-parse
        session-from-directed
        splice-session-index
        make-incremental-session)

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

