;;; Complete requests and prepared recognition at matched source scales.
(import (only-in :gerbil-parser/languages/gql/parser gql-parser parse-gql)
        (only-in :gerbil-parser/languages/fhirpath/parser fhirpath-parser parse-fhirpath)
        (only-in :gerbil-parser/languages/arithmetic/parser arithmetic-parser parse-arithmetic)
        (rename-in (only-in :gerbil-parser/t/benchmarks/gql/runtime/matched-stages measure-gql-component)
                   (measure-gql-component measure-parser-component))
        (only-in :gerbil-parser/src/compiler/machine parser-machine-runtime
                 parser-machine-grammar-digest parser-machine-trivia)
        (only-in :gerbil-parser/src/runtime/lr-parser lr-parse/prepared lr-parse/prepared/receipt)
        (only-in :gerbil-parser/src/runtime/significant parser-significant-tokens)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/src/runtime/artifact make-success-parse-artifact
                 parse-artifact-events parse-artifact-valid-for-source?
                 token-event? token-event-token-kind token-event-lexeme event-start event-end))
(export benchmark-lr-request-stack)
(def (benchmark-family family units machine parse source samples iterations)
  (let* ((reference (parse source))
         (tokens (map (lambda (event)
                        (make-token (token-event-token-kind event) (token-event-lexeme event)
                                    (event-start event) (event-end event)))
                      (filter token-event? (parse-artifact-events reference))))
         (significant (parser-significant-tokens machine tokens))
         (runtime (parser-machine-runtime machine))
         (parsed (call-with-values (lambda () (lr-parse/prepared runtime significant)) list)))
    (unless (and (parse-artifact-valid-for-source? reference source) (null? (cadr parsed)))
      (error "invalid complete stack benchmark product" family units))
    (let-values (((root rest receipt) (lr-parse/prepared/receipt runtime significant)))
      (unless (and (null? rest)
                   (equal? parsed (list root rest))
                   (equal? reference
                     (make-success-parse-artifact (parser-machine-grammar-digest machine)
                       source tokens root (parser-machine-trivia machine))))
        (error "independent GLR product mismatch" family units)))
    (write (list 'LR-STACK-WORKLOAD family units 'source-characters (string-length source)
                 'tokens (length significant))) (newline) (force-output)
    (measure-parser-component (list family units 'prepared-lr) samples iterations
      (lambda () (call-with-values (lambda () (lr-parse/prepared runtime significant)) list)) parsed)
    (measure-parser-component (list family units 'full-source) samples iterations
      (lambda () (parse source)) reference)))
(def (benchmark-lr-request-stack (samples 20) (iterations 20))
  (unless (and (integer? samples) (positive? samples)
               (integer? iterations) (positive? iterations))
    (error "stack benchmark requires positive sample and iteration counts"))
  (for-each
   (lambda (units)
     (benchmark-family 'gql-return units gql-parser parse-gql
       (string-append "RETURN "
         (string-join (map (lambda (i) (string-append "v" (number->string i))) (iota units)) ", ")
         "\n") samples iterations)
     (let (source (string-join (make-list units "1") " + "))
       (benchmark-family 'fhirpath-addition units fhirpath-parser parse-fhirpath source samples iterations)
       (benchmark-family 'arithmetic-addition units arithmetic-parser parse-arithmetic source samples iterations)))
   '(64 512))
  (displayln "LR-REQUEST-STACK-OK") (force-output))
