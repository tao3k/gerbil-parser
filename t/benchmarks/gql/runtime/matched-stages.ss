#!/usr/bin/env gxi
;;; GQL workload selection; sampling and admission belong to the shared owner.
;;; Prepared directed-token controls are not an additive source-cost breakdown.
(import (only-in ./execution-counts profile-gql-prepared-execution)
        (only-in ./reduction-counts profile-gql-reductions)
        (only-in ../../parser-stage-cost/benchmark measure-parser-component)
        :gerbil-parser/languages/gql/parser
        :gerbil-parser/src/compiler/machine
        :gerbil-parser/src/runtime/lexer
        :gerbil-parser/src/runtime/lr-parser
        :gerbil-parser/src/runtime/significant
        :gerbil-parser/src/runtime/artifact
        :gerbil-parser/src/runtime/token)
(export main profile-gql-stages)

(def (main . args)
  (let ((samples (if (pair? args) (string->number (car args)) 40))
        (batch-count (if (> (length args) 1) (string->number (cadr args)) 100)))
    (profile-gql-stages +gql-representative-query+ samples batch-count)
    (display "GQL-STAGES-OK\n") (force-output)))

(def (profile-gql-stages source samples batch-count)
    (unless (and (integer? samples) (positive? samples)
                 (integer? batch-count) (positive? batch-count))
      (error "expected positive sample and batch counts" samples batch-count))
    (let* ((reference (parse-gql source))
           (tokens
            (map (lambda (event)
                   (make-token (token-event-token-kind event)
                               (token-event-lexeme event)
                               (event-start event) (event-end event)))
                 (filter token-event? (parse-artifact-events reference))))
           (significant (parser-significant-tokens gql-parser tokens))
           (runtime (parser-machine-runtime gql-parser))
           (lexed (lex-source gql-parser source))
           (parsed (call-with-values
                    (lambda () (lr-parse/prepared runtime significant)) list)))
      (unless (and (parse-artifact-success? reference)
                   (parse-artifact-valid? reference) (null? (cadr parsed))
                   (equal? (make-success-parse-artifact
                            (parser-machine-grammar-digest gql-parser)
                            source tokens (car parsed)
                            (parser-machine-trivia gql-parser)) reference))
        (error "prepared GQL stages do not reproduce the directed artifact"))
      ;; Observer work stays outside measurement and leaves public artifacts intact.
      (write (list 'GQL-LR-EXECUTION (profile-gql-prepared-execution source)))
      (newline) (force-output)
      (write (list 'GQL-REDUCTION-COUNTS (profile-gql-reductions source)))
      (newline) (force-output)
      (write (list (cons 'sourceBytes (parse-artifact-ref reference 'sourceByteLength))
                   (cons 'tokens (length tokens))
                   (cons 'significantTokens (length significant))))
      (newline) (force-output)
      (list
        (measure-parser-component 'global-lexing samples batch-count
               (lambda () (lex-source gql-parser source)) lexed)
       (measure-parser-component 'prepared-lr samples batch-count
               (lambda () (call-with-values
                           (lambda () (lr-parse/prepared runtime significant)) list))
               parsed)
       (measure-parser-component 'artifact-publication samples batch-count
               (lambda () (make-success-parse-artifact
                           (parser-machine-grammar-digest gql-parser)
                           source tokens (car parsed)
                           (parser-machine-trivia gql-parser))) reference)
       (measure-parser-component 'full-source samples batch-count
               (lambda () (parse-gql source)) reference)
       )))
