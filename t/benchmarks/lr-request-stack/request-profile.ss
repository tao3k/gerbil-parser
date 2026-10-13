(import (only-in :gerbil-parser/languages/gql/parser gql-parser parse-gql +gql-representative-query+)
        (only-in :gerbil-parser/src/compiler/machine parser-machine-runtime parser-machine-ir)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-events token-event?
                 token-event-token-kind token-event-lexeme event-start event-end)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in ./lookahead profile-lr-lookahead)
        (only-in ./derivation profile-lr-request-reductions prepare-lr-semantic-replay
                 profile-lr-semantic-actions)
        (only-in :gerbil-parser/src/runtime/lr-parser lr-parse/prepared)
        (only-in :gerbil-parser/src/runtime/lexer lex-source)
        (only-in :gerbil-parser/src/runtime/significant parser-significant-tokens))
(export main profile-work)
;;; Statistical attribution is independent of the matched CPU admission.
;;; The workload does not assume a profiler frontend is installed.
(def (main (stage "source"))
  (unless (member stage '("source" "prepared"))
    (error "invalid request profile stage" stage))
  (let* ((source +gql-representative-query+)
         (runtime (parser-machine-runtime gql-parser))
         (tokens (parser-significant-tokens gql-parser (lex-source gql-parser source)))
         (request (if (equal? stage "prepared")
                    (lambda () (call-with-values (lambda () (lr-parse/prepared runtime tokens)) list))
                    (lambda () (parse-gql source))))
         (expected (request)))
    (let loop ((remaining 30000) (result expected))
      (if (zero? remaining)
        (unless (equal? result expected) (error "profile changed complete request" stage))
        (begin
          (when (zero? (modulo remaining 3000)) (displayln "PROFILE-REQUESTS " remaining) (force-output))
          (loop (fx- remaining 1) (request)))))
    (displayln "REQUEST-PROFILE-OK") (force-output)))


;;; Count work from the actual complete product outside every timed interval.
;;; Replay validates canonical operands; these counts are not CPU percentages.
(def (profile-work)
  (let* ((source +gql-representative-query+) (expected (parse-gql source))
         (tokens (map (lambda (event)
                        (make-token (token-event-token-kind event) (token-event-lexeme event)
                                    (event-start event) (event-end event)))
                      (filter token-event? (parse-artifact-events expected))))
         (significant (parser-significant-tokens gql-parser tokens))
         (runtime (parser-machine-runtime gql-parser))
         (spec (cdr (assq 'lr-spec (parser-machine-ir gql-parser)))))
    (write (list 'LR-REQUEST-WORK 'gql-request
                 (profile-lr-lookahead gql-parser source expected)
                 (profile-lr-request-reductions gql-parser significant)
                 (profile-lr-semantic-actions
                  (prepare-lr-semantic-replay runtime spec significant))))
    (newline) (displayln "REQUEST-WORK-OK") (force-output)))
