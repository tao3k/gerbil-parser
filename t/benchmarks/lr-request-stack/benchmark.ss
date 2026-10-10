;;; Complete requests and prepared recognition at matched source scales.
(import (only-in ./derivation profile-lr-request-reductions)
        (only-in :gerbil-parser/t/scenarios/performance/selective-glr/scenario
                 selective-glr-scenario selective-glr-scenario-pass?)
        (only-in :gerbil-parser/languages/gql/parser gql-parser parse-gql)
        (only-in :gerbil-parser/languages/fhirpath/parser fhirpath-parser parse-fhirpath)
        (only-in :gerbil-parser/languages/arithmetic/parser arithmetic-parser parse-arithmetic)
        (rename-in (only-in :gerbil-parser/t/benchmarks/gql/runtime/matched-stages measure-gql-component)
                   (measure-gql-component measure-parser-component))
        (only-in :gerbil-parser/src/compiler/machine parser-machine-runtime parser-machine-ir
                 parser-machine-grammar-digest parser-machine-trivia
                 parser-machine-direct-drive parser-machine-direct-source)
        (only-in :gerbil-parser/src/runtime/lr-parser lr-parse/prepared lr-parse/prepared/receipt
                 lr-prepare lr-runtime-for-current-semantic-backend
                 current-lr-event-program-enabled? lr-runtime-event-program?)
        (only-in :gerbil-parser/src/runtime/parse-cost current-parser-cost-observer)
        (only-in :gerbil-parser/src/runtime/significant parser-significant-tokens)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/src/runtime/artifact make-success-parse-artifact
                 parse-artifact-events parse-artifact-valid-for-source?
                 token-event? token-event-token-kind token-event-lexeme event-start event-end))
(export benchmark-lr-request-stack benchmark-lr-request-backends
        benchmark-lr-completions benchmark-lr-semantic-preparation)

;;; Preparation includes eligibility and generic selection. Repeated selection
;;; uses the prepared runtime, including negative admission. Neither stage is a
;;; decomposition of full parsing or a before/after performance claim.
(def (benchmark-lr-semantic-preparation (samples 20) (preparations 10) (selections 1000))
  (parameterize ((current-lr-event-program-enabled? #t))
    (for-each
     (lambda (entry)
       (let* ((family (car entry))
              (spec (cdr (assq 'lr-spec (parser-machine-ir (cdr entry)))))
              (runtime (lr-prepare spec))
              (select (lambda (runtime)
                        (lr-runtime-event-program?
                         (lr-runtime-for-current-semantic-backend runtime))))
              (expected (select runtime)))
         (displayln "LR-SEMANTIC-ADMISSION " family " event=" expected) (force-output)
         (measure-parser-component (list family 'semantic-preparation) samples preparations
                                   (lambda () (select (lr-prepare spec))) expected)
         (measure-parser-component (list family 'prepared-backend-selection) samples selections
                                   (lambda () (select runtime)) expected)))
     (list (cons 'gql gql-parser) (cons 'fhirpath fhirpath-parser)
           (cons 'arithmetic arithmetic-parser))))
  (displayln "LR-SEMANTIC-PREPARATION-OK") (force-output))

;;; A complete group contains equivalent merge, dynamic ranking, fragment
;;; interning and typed ambiguity rejection. Machine preparation is excluded.
(def (benchmark-lr-completions (samples 20) (iterations 20))
  (let (reference (selective-glr-scenario))
    (unless (selective-glr-scenario-pass? reference)
      (error "invalid GLR completion benchmark product"))
    (measure-parser-component 'selective-glr-completion samples iterations
                              selective-glr-scenario reference)
    (displayln "LR-COMPLETION-OK") (force-output)))
(def (benchmark-family family units machine parse source samples iterations)
  (let* ((reference (parameterize ((current-lr-event-program-enabled? #f)) (parse source)))
         (tokens (map (lambda (event)
                        (make-token (token-event-token-kind event) (token-event-lexeme event)
                                    (event-start event) (event-end event)))
                      (filter token-event? (parse-artifact-events reference))))
         (significant (parser-significant-tokens machine tokens))
         (materialized-runtime (parser-machine-runtime machine))
         (runtime (lr-runtime-for-current-semantic-backend materialized-runtime))
         (parsed (call-with-values (lambda () (lr-parse/prepared runtime significant)) list)))
    (unless (and (parse-artifact-valid-for-source? reference source) (null? (cadr parsed)))
      (error "invalid complete stack benchmark product" family units))
    (let-values (((root rest receipt) (lr-parse/prepared/receipt materialized-runtime significant)))
      (unless (and (null? rest)
                   (equal? reference
                     (make-success-parse-artifact (parser-machine-grammar-digest machine)
                       source tokens (car parsed) (parser-machine-trivia machine)))
                   (equal? reference
                     (make-success-parse-artifact (parser-machine-grammar-digest machine)
                       source tokens root (parser-machine-trivia machine))))
        (error "independent GLR product mismatch" family units)))
    ;; Cost observation preserves the fresh public route. This untimed witness
    ;; records actual source stages; prepared admission alone cannot establish
    ;; which executor a generated source/drive path used.
    (let (stages '())
      (let (witness
            (parameterize
             ((current-parser-cost-observer
               (lambda (name receipt)
                 (set! stages (cons (cons name (cdr (assq 'completed receipt))) stages)))))
             (parse source)))
        (unless (equal? witness reference)
          (error "source route witness changed complete product" family units)))
      (write (list 'LR-REQUEST-BACKEND family units
                   'requested-event (current-lr-event-program-enabled?)
                   'prepared-event (lr-runtime-event-program? runtime)
                   'generated-drive (and (parser-machine-direct-drive machine) #t)
                   'generated-source (and (parser-machine-direct-source machine) #t)
                   'source-stages (reverse stages))))
    (newline) (force-output)
    (write (list 'LR-STACK-WORKLOAD family units 'source-characters (string-length source)
                 'tokens (length significant))) (newline) (force-output)
    (write (list 'LR-REQUEST-REDUCTIONS family units
                 (parameterize ((current-lr-event-program-enabled? #f))
                   (profile-lr-request-reductions machine significant))))
    (newline) (force-output)
    (measure-parser-component (list family units 'prepared-lr) samples iterations
      (lambda () (call-with-values (lambda () (lr-parse/prepared runtime significant)) list)) parsed)
    (measure-parser-component (list family units 'full-source) samples iterations
      (lambda () (parse source)) reference)))
(def (benchmark-request-families samples iterations label)
  (unless (and (integer? samples) (positive? samples)
               (integer? iterations) (positive? iterations))
    (error "stack benchmark requires positive sample and iteration counts"))
  (for-each
   (lambda (units)
     (benchmark-family (label 'gql-return) units gql-parser parse-gql
       (string-append "RETURN "
         (string-join (map (lambda (i) (string-append "v" (number->string i))) (iota units)) ", ")
         "\n") samples iterations)
     (let (source (string-join (make-list units "1") " + "))
       (benchmark-family (label 'fhirpath-addition) units fhirpath-parser parse-fhirpath source samples iterations)
       (benchmark-family (label 'arithmetic-addition) units arithmetic-parser parse-arithmetic source samples iterations)))
   '(64 512)))

(def (benchmark-lr-request-stack (samples 20) (iterations 20))
  (benchmark-request-families samples iterations identity)
  (displayln "LR-REQUEST-STACK-OK") (force-output))

;;; Compare semantic representations across complete public requests. Every
;;; route publishes the recognition reference, including grammar rejection of
;;; the requested event backend and independent generated source/drive routes.
;;; Prepared roots may differ in representation;
;;; their canonical artifacts must agree before any timed batch is admitted.
(def (benchmark-lr-request-backends (samples 20) (iterations 20))
  (for-each
   (lambda (events?)
     (parameterize ((current-lr-event-program-enabled? events?))
       (benchmark-request-families samples iterations
         (lambda (family) (list family (if events? 'event 'recognition))))))
   '(#f #t))
  (displayln "LR-REQUEST-BACKENDS-OK") (force-output))
