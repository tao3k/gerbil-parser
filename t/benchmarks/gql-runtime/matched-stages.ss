#!/usr/bin/env gxi
;;; Informational CPU/wall samples; the existing ASP contract owns admission.
;;; Prepared stages use the directed parser's token kinds, not global lexing.
;;; Their costs are not a decomposition of the streaming full-source path.
(import :gerbil-parser/languages/gql/iso-39075-2024/parser
        :gerbil-parser/src/compiler/machine
        :gerbil-parser/src/runtime/lexer
        :gerbil-parser/src/runtime/lr-parser
        :gerbil-parser/src/runtime/significant
        :gerbil-parser/src/runtime/artifact
        :gerbil-parser/src/runtime/token)
(export main)

(def (measure name samples batch-count thunk expected)
  (let loop ((sample 0))
    (when (< sample samples)
      (##gc)
      (let* ((cpu-start (cpu-time)) (wall-start (##current-time-point))
             (result
              (let repeat ((remaining batch-count) (last-result #f))
                (if (zero? remaining) last-result
                  (repeat (- remaining 1) (thunk)))))
             (cpu-ms (* 1000 (- (cpu-time) cpu-start)))
             (wall-ms (* 1000 (- (##current-time-point) wall-start))))
        (unless (equal? result expected)
          (error "GQL stage changed its semantic result" name sample))
        (write (list name (cons 'sample sample) (cons 'parses batch-count)
                     (cons 'cpu-ms cpu-ms) (cons 'elapsed-ms wall-ms)))
        (newline) (force-output))
      (loop (+ sample 1)))))

(def (main . args)
  (let ((samples (if (pair? args) (string->number (car args)) 3))
        (batch-count (if (> (length args) 1) (string->number (cadr args)) 100))
        (source +gql-representative-query+))
    (unless (and (integer? samples) (positive? samples)
                 (integer? batch-count) (positive? batch-count))
      (error "expected positive sample and batch counts" args))
    (let* ((reference (parse-gql-iso-39075-2024 source))
           (tokens
            (map (lambda (event)
                   (make-token (token-event-token-kind event)
                               (token-event-lexeme event)
                               (event-start event) (event-end event)))
                 (filter token-event? (parse-artifact-events reference))))
           (significant (parser-significant-tokens gql-iso-parser tokens))
           (runtime (parser-machine-runtime gql-iso-parser))
           (lexed (lex-source gql-iso-parser source))
           (parsed (call-with-values
                    (lambda () (lr-parse/prepared runtime significant)) list)))
      (unless (and (parse-artifact-success? reference)
                   (parse-artifact-valid? reference) (null? (cadr parsed))
                   (equal? (make-success-parse-artifact
                            (parser-machine-grammar-digest gql-iso-parser)
                            source tokens (car parsed)
                            (parser-machine-trivia gql-iso-parser)) reference))
        (error "prepared GQL stages do not reproduce the directed artifact"))
      (write (list (cons 'sourceBytes (parse-artifact-ref reference 'sourceByteLength))
                   (cons 'tokens (length tokens))
                   (cons 'significantTokens (length significant))))
      (newline) (force-output)
      (measure 'global-lexing samples batch-count
               (lambda () (lex-source gql-iso-parser source)) lexed)
      (measure 'prepared-lr samples batch-count
               (lambda () (call-with-values
                           (lambda () (lr-parse/prepared runtime significant)) list))
               parsed)
      (measure 'artifact-publication samples batch-count
               (lambda () (make-success-parse-artifact
                           (parser-machine-grammar-digest gql-iso-parser)
                           source tokens (car parsed)
                           (parser-machine-trivia gql-iso-parser))) reference)
      (measure 'full-source samples batch-count
               (lambda () (parse-gql-iso-39075-2024 source)) reference)
      (display "GQL-STAGES-OK\n") (force-output))))
