;;; Untimed diagnostics from the same prepared LR executor, one action at a time.
(import :gerbil-parser/languages/gql/parser
        (only-in :gerbil-parser/src/compiler/machine parser-machine-ir parser-machine-runtime
                 parser-machine-grammar-digest parser-machine-trivia)
        (only-in :gerbil-parser/src/compiler/lr lr-spec-ref)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success? parse-artifact-valid? parse-artifact-events
                 make-success-parse-artifact
                 token-event? token-event-token-kind token-event-lexeme event-start event-end)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/src/runtime/significant parser-significant-tokens)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 lr-parse/prepared lr-initial-checkpoint lr-checkpoint-advance
                 lr-checkpoint-state lr-checkpoint-deterministic-actions
                 lr-checkpoint-deterministic-shifts lr-checkpoint-remaining-token-count))
(export profile-gql-prepared-execution)

(def (profile-gql-prepared-execution source)
  (let* ((reference (parse-gql source))
         (runtime (parser-machine-runtime gql-parser))
         (spec (cdr (assq 'lr-spec (parser-machine-ir gql-parser))))
         (rows (lr-spec-ref spec 'actions)))
    (unless (and (parse-artifact-success? reference) (parse-artifact-valid? reference))
      (error "execution diagnostics require an accepted GQL artifact"))
    (let* ((tokens
            (map (lambda (event)
                   (make-token (token-event-token-kind event) (token-event-lexeme event)
                               (event-start event) (event-end event)))
                 (filter token-event? (parse-artifact-events reference))))
           (significant (parser-significant-tokens gql-parser tokens))
           (expected (call-with-values (lambda () (lr-parse/prepared runtime significant)) list)))
      (unless (and (null? (cadr expected))
                   (equal? (make-success-parse-artifact
                            (parser-machine-grammar-digest gql-parser)
                            source tokens (car expected) (parser-machine-trivia gql-parser))
                           reference))
        (error "prepared execution changed the directed artifact"))
      (let loop ((checkpoint (lr-initial-checkpoint runtime significant))
                 (non-eof 0) (eof 0) (literal-free 0))
        (let* ((remaining (lr-checkpoint-remaining-token-count checkpoint))
               (row (vector-ref rows (lr-checkpoint-state checkpoint)))
               (next-non-eof (+ non-eof (if (zero? remaining) 0 1)))
               (next-eof (+ eof (if (zero? remaining) 1 0)))
               (next-literal-free
                (+ literal-free
                   (if (and (positive? remaining)
                            (not (any (lambda (entry) (eq? (cadar entry) 'literal)) row)))
                     1 0))))
          (let-values (((status next) (lr-checkpoint-advance checkpoint 1)))
            (case status
              ((checkpoint)
               (let ((actions (- (lr-checkpoint-deterministic-actions next)
                                 (lr-checkpoint-deterministic-actions checkpoint)))
                     (shifts (- (lr-checkpoint-deterministic-shifts next)
                                (lr-checkpoint-deterministic-shifts checkpoint))))
                 (unless (and (= actions 1) (memv shifts '(0 1)))
                   (error "checkpoint did not execute exactly one LR action"))
                 (loop next next-non-eof next-eof next-literal-free)))
              ((accepted)
               ;; Fallback may execute a whole suffix within one call. Only a
               ;; checkpoint already at the explicit EOF accept entry qualifies.
               (let (accept-entry (find (lambda (entry) (eq? (cadar entry) 'eof)) row))
                 (unless (and (zero? remaining) accept-entry
                              (eq? (cadr accept-entry) 'accept) (equal? next expected))
                   (error "single-action execution did not reach the same EOF acceptance")))
               (let ((actions (lr-checkpoint-deterministic-actions checkpoint))
                     (shifts (lr-checkpoint-deterministic-shifts checkpoint)))
                 (list (cons 'scope 'prepared-lr-single-action-checkpoints)
                       (cons 'actions actions) (cons 'shifts shifts)
                       (cons 'reductions (- actions shifts)) (cons 'accepts 1)
                       (cons 'nonEofObservations next-non-eof)
                       (cons 'eofObservations next-eof)
                       (cons 'literalFreeNonEofObservations next-literal-free))))
              (else (error "unexpected single-action execution status" status)))))))))
