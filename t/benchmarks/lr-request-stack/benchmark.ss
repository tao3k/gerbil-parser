;;; Complete requests and prepared recognition at matched source scales.
(import (only-in ./derivation profile-lr-request-reductions)
        (only-in :gerbil-parser/t/scenarios/performance/selective-glr/scenario
                 selective-glr-scenario selective-glr-scenario-pass?)
        (only-in :gerbil-parser/languages/gql/parser gql-parser parse-gql)
        (only-in :gerbil-parser/languages/fhirpath/parser fhirpath-parser parse-fhirpath)
        (only-in :gerbil-parser/languages/arithmetic/parser arithmetic-parser parse-arithmetic)
        (rename-in (only-in :gerbil-parser/t/benchmarks/gql/runtime/matched-stages measure-gql-component measure-parser-batch sample-at-percentile)
                   (measure-gql-component measure-parser-component))
        (only-in :gerbil-parser/src/compiler/machine parser-machine-runtime parser-machine-ir
                 parser-machine-grammar-digest parser-machine-trivia
                 parser-machine-direct-drive parser-machine-direct-source
                 parser-machine-backend-representation parser-machine-for-current-semantic-backend)
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
(def (benchmark-family family units machine parse source samples iterations
                       (measure measure-parser-component))
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
      (let* ((expected-route (if (parser-machine-direct-drive machine)
                              'generated-drive-execution 'prepared-checkpoint-execution))
             (completed (assq expected-route stages)))
        (unless (and completed (cdr completed))
          (error "complete request did not execute its expected engine route" family units expected-route)))
      (write (list 'LR-REQUEST-BACKEND family units
                   'requested-event (current-lr-event-program-enabled?)
                   'prepared-event (lr-runtime-event-program? runtime)
                   'generated-drive (and (parser-machine-direct-drive machine) #t)
                   'generated-source (and (parser-machine-direct-source machine) #t)
                   'prepared-representation
                   (parser-machine-backend-representation
                    (parser-machine-for-current-semantic-backend machine) 'prepared)
                   'generated-drive-representation (parser-machine-backend-representation machine 'drive)
                   'generated-source-representation (parser-machine-backend-representation machine 'source)
                   'source-stages (reverse stages))))
    (newline) (force-output)
    (write (list 'LR-STACK-WORKLOAD family units 'source-characters (string-length source)
                 'tokens (length significant))) (newline) (force-output)
    (write (list 'LR-REQUEST-REDUCTIONS family units
                 (parameterize ((current-lr-event-program-enabled? #f))
                   (profile-lr-request-reductions machine significant))))
    (newline) (force-output)
    (measure (list family units 'prepared-lr) samples iterations
      (lambda () (call-with-values (lambda () (lr-parse/prepared runtime significant)) list)) parsed)
    (measure (list family units 'full-source) samples iterations
      (lambda () (parse source)) reference)))
(def (benchmark-request-families samples iterations label
                                 (measure measure-parser-component))
  (unless (and (integer? samples) (positive? samples)
               (integer? iterations) (positive? iterations))
    (error "stack benchmark requires positive sample and iteration counts"))
  (for-each
   (lambda (units)
     (benchmark-family (label 'gql-return) units gql-parser parse-gql
       (string-append "RETURN "
         (string-join (map (lambda (i) (string-append "v" (number->string i))) (iota units)) ", ")
         "\n") samples iterations measure)
     (let (source (string-join (make-list units "1") " + "))
       (benchmark-family (label 'fhirpath-addition) units fhirpath-parser parse-fhirpath source samples iterations measure)
       (benchmark-family (label 'arithmetic-addition) units arithmetic-parser parse-arithmetic source samples iterations measure)))
   '(64 512)))

(def (benchmark-lr-request-stack (samples 20) (iterations 20))
  (benchmark-request-families samples iterations identity)
  (displayln "LR-REQUEST-STACK-OK") (force-output))

;;; Compare semantic representations across complete public requests. Every
;;; route publishes the recognition reference, including grammar rejection of
;;; the requested event backend and independent generated source/drive routes.
;;; Prepared roots may differ in representation;
;;; their canonical artifacts must agree before any timed batch is admitted.
(defstruct request-variant (events? thunk expected))

(def (batch-value row key) (cdr (assq key row)))
(def (paired-percentile rows key rank)
  (batch-value (sample-at-percentile rows key rank) key))

;;; Empirical observation band, not a confidence interval or causal proof.
;;; A practical 5% margin is fixed before sampling; natural GC stays in CPU.
(def (paired-decision rows)
  (cond ((< (paired-percentile rows 'cpu-ratio 90) 0.95) 'observed-benefit)
        ((> (paired-percentile rows 'cpu-ratio 10) 1.05) 'observed-regression)
        (else 'not-admitted)))

(def (measure-backend-pair name variants groups iterations)
  (def (run variant sample)
    (parameterize ((current-lr-event-program-enabled? (request-variant-events? variant)))
      (measure-parser-batch name sample iterations
        (request-variant-thunk variant) (request-variant-expected variant))))
  ;; Warm both admitted representations before one initial collection. Variant
  ;; construction, derivation, source witnesses and validation are untimed.
  (run (car variants) -1) (run (cadr variants) -1) (##gc)
  (let loop ((group 0) (observations '()))
    (if (= group groups)
      (let* ((rows (reverse observations))
             (allocated (filter (lambda (row) (number? (batch-value row 'allocation-ratio))) rows))
             (summary
              (list (cons 'workload name) (cons 'groups groups)
                    (cons 'calls-per-batch iterations)
                    (cons 'decision (paired-decision rows))
                    (cons 'cpu-ratio-p10 (paired-percentile rows 'cpu-ratio 10))
                    (cons 'cpu-ratio-p50 (paired-percentile rows 'cpu-ratio 50))
                    (cons 'cpu-ratio-p90 (paired-percentile rows 'cpu-ratio 90))
                    (cons 'recognition-cpu-ms-per-call
                          (paired-percentile rows 'recognition-cpu-ms-per-call 50))
                    (cons 'event-cpu-ms-per-call
                          (paired-percentile rows 'event-cpu-ms-per-call 50))
                    (cons 'allocation-admitted-groups
                          (length allocated))
                    (cons 'allocation-ratio-p50
                          (and (pair? allocated) (paired-percentile allocated 'allocation-ratio 50)))
                    (cons 'gc-collections
                          (apply + (map (lambda (row)
                                          (apply + (map (lambda (entry) (batch-value (cdr entry) 'gc-count))
                                                        (batch-value row 'batches)))) rows)))
                    (cons 'observations rows))))
        (write (list 'LR-BACKEND-PAIR-SUMMARY
                     (filter (lambda (entry) (not (eq? (car entry) 'observations))) summary)))
        (newline) (force-output)
        summary)
      (let* ((order (if (even? group) '(0 1 1 0) '(1 0 0 1)))
             (batches (map (lambda (index)
                            (cons index (run (list-ref variants index) group))) order))
             (selected (lambda (index key)
                         (map (lambda (entry) (batch-value (cdr entry) key))
                              (filter (lambda (entry) (= (car entry) index)) batches)))))
        (def (sum index key) (apply + (selected index key)))
        (let* ((recognition (sum 0 'cpu-ms)) (events (sum 1 'cpu-ms))
               (allocations (map (lambda (entry) (batch-value (cdr entry) 'allocated-bytes)) batches)))
          (unless (and (positive? recognition) (positive? events))
            (error "paired benchmark CPU resolution is insufficient" name group))
          (let (row
                (list (cons 'sample group) (cons 'order order)
                      (cons 'cpu-ratio (/ events recognition))
                      (cons 'recognition-cpu-ms-per-call (/ recognition (* 2 iterations)))
                      (cons 'event-cpu-ms-per-call (/ events (* 2 iterations)))
                      (cons 'allocation-ratio
                            (and (every number? allocations)
                                 (positive? (sum 0 'allocated-bytes))
                                 (/ (sum 1 'allocated-bytes) (sum 0 'allocated-bytes))))
                      (cons 'batches batches)))
            (write (list 'LR-BACKEND-PAIR-SAMPLE name row)) (newline) (force-output)
            (loop (+ group 1) (cons row observations))))))))

(def (benchmark-lr-request-backends (groups 20) (iterations 20))
  (unless (and (integer? groups) (exact? groups) (>= groups 20)
               (integer? iterations) (exact? iterations) (positive? iterations))
    (error "matched backend qualification requires at least 20 groups and positive integral calls"))
  (let ((catalog (make-hash-table)) (keys '()))
    (for-each
     (lambda (events?)
       (parameterize ((current-lr-event-program-enabled? events?))
         (benchmark-request-families groups iterations identity
           (lambda (key _samples _iterations thunk expected)
             (let (variant (make-request-variant events? thunk expected))
               (if events?
                 (hash-put! catalog key (list (car (hash-ref catalog key)) variant))
                 (begin (set! keys (cons key keys))
                        (hash-put! catalog key (list variant)))))))))
     '(#f #t))
    (let* ((summaries (map (lambda (key)
                            (measure-backend-pair key (hash-ref catalog key) groups iterations))
                          (reverse keys)))
           (controls (filter (lambda (row)
                               (eq? (car (batch-value row 'workload)) 'gql-return)) summaries))
           (stable? (every (lambda (row)
                            (and (<= (batch-value row 'cpu-ratio-p10) 1)
                                 (>= (batch-value row 'cpu-ratio-p90) 1)
                                 (<= 0.95 (batch-value row 'cpu-ratio-p50) 1.05))) controls)))
      (write (list 'LR-BACKEND-CONTROL-ADMISSION 'gql-fallback-stable stable?))
      (newline) (force-output)
      (write (list 'LR-BACKEND-PERFORMANCE-ADMISSION
                   'control-stable stable?
                   'complete-request-benefit-admitted
                   (and stable?
                        (every (lambda (row)
                                 (eq? (batch-value row 'decision) 'observed-benefit))
                               (filter (lambda (row)
                                         (let (key (batch-value row 'workload))
                                           (and (not (eq? (car key) 'gql-return))
                                                (eq? (caddr key) 'full-source)))) summaries)))))
      (newline) (force-output)
      (displayln "LR-REQUEST-BACKENDS-OK") (force-output)
      summaries)))
