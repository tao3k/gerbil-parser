;;; Real-request boundary diagnostics; the four-stage CPU gate is separate.
(import (only-in :gerbil-parser/languages/gql/parser
                 gql-parser parse-gql +gql-representative-query+)
        (only-in :gerbil-parser/src/runtime/machine
                 parser-machine-runtime parser-machine-grammar-digest parser-machine-trivia)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 lr-parse/prepared lr-parse/prepared/receipt lr-initial-checkpoint lr-checkpoint-drive)
        (only-in :gerbil-parser/src/runtime/significant parser-significant-tokens)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/src/runtime/artifact
                 make-success-parse-artifact parse-artifact-valid-for-source? parse-artifact-events
                 token-event? token-event-token-kind token-event-lexeme event-start event-end)
        (only-in ../parser-stage-cost/benchmark measure-parser-component))
(export main)

;;; Supplies the admitted significant tokens through the production input
;;; callback protocol. No scanner or token observer is timed in this control.
(def (recognize-stream runtime tokens)
  (let (remaining tokens)
    (def (next-input _mode)
      (and (pair? remaining)
           (let (input (car remaining))
             (set! remaining (cdr remaining)) input)))
    (let-values (((status payload)
                  (lr-checkpoint-drive (lr-initial-checkpoint runtime '()) next-input #f)))
      (unless (and (eq? status 'accepted) (null? remaining))
        (error "boundary diagnostic requires complete deterministic recognition" status))
      payload)))

(def (main)
  (let* ((source +gql-representative-query+) (machine gql-parser)
         (artifact (parse-gql source))
         (tokens (map (lambda (event)
                        (make-token (token-event-token-kind event) (token-event-lexeme event)
                                    (event-start event) (event-end event)))
                      (filter token-event? (parse-artifact-events artifact))))
         (runtime (parser-machine-runtime machine))
         (significant (parser-significant-tokens machine tokens))
         (prepared (lambda () (call-with-values (lambda () (lr-parse/prepared runtime significant)) list)))
         (recognized (prepared))
         (digest (parser-machine-grammar-digest machine))
         (trivia? (parser-machine-trivia machine)))
    (def (publish root) (make-success-parse-artifact digest source tokens root trivia?))
    (unless (and (parse-artifact-valid-for-source? artifact source)
                 (null? (cadr recognized)) (equal? (publish (car recognized)) artifact)
                 (equal? (recognize-stream runtime significant) recognized))
      (error "request boundary diagnostic changed the complete product"))
    (let-values (((root rest _receipt) (lr-parse/prepared/receipt runtime significant)))
      (unless (and (null? rest) (equal? (publish root) artifact))
        (error "independent GLR boundary product differs")))
    ;; Separate component intervals are diagnostic. Their medians/GC observations
    ;; must not be subtracted to claim an additive decomposition or CPU benefit.
    (measure-parser-component '(gql-request boundary-prepared-lr) 40 1000 prepared recognized)
    (measure-parser-component '(gql-request boundary-streamed-lr) 40 1000
      (lambda () (recognize-stream runtime significant)) recognized)
    (measure-parser-component '(gql-request boundary-publication) 40 1000
      (lambda () (publish (car recognized))) artifact)
    (measure-parser-component '(gql-request boundary-lr-publication) 40 1000
      (lambda () (let-values (((root rest) (lr-parse/prepared runtime significant)))
                   (unless (null? rest) (error "unexpected boundary remainder")) (publish root))) artifact)
    (measure-parser-component '(gql-request boundary-full-source) 40 1000
      (lambda () (parse-gql source)) artifact)
    (displayln "REQUEST-COST-BOUNDARIES-OK") (force-output)))
