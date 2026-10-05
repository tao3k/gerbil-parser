#!/usr/bin/env gxi
;;; Informational CPU/wall samples; the existing ASP contract owns admission.
;;; Prepared stages use the directed parser's token kinds, not global lexing.
;;; Their costs are not a decomposition of the streaming full-source path.
(import (only-in ./execution-counts profile-gql-prepared-execution)
        (only-in ./reduction-counts profile-gql-reductions)
        (only-in :asp-gerbil-scheme/src/benchmark/statistics benchmark-percentile-index)
        :gerbil-parser/languages/gql/parser
        :gerbil-parser/src/compiler/machine
        :gerbil-parser/src/runtime/lexer
        :gerbil-parser/src/runtime/lr-parser
        :gerbil-parser/src/runtime/significant
        :gerbil-parser/src/runtime/artifact
        :gerbil-parser/src/runtime/token)
(export main profile-gql-stages measure-gql-component sample-at-percentile gc-statistics-snapshot sample-gc-snapshot)

(def (percentile rows key rank)
  (let (values (list-sort < (map (lambda (row) (cdr (assq key row))) rows)))
    (list-ref values (benchmark-percentile-index (length values) rank))))

;; Select the actual wall-ranked observation, preserving its CPU/GC counters.
;; Sample number breaks ties deterministically; independent CPU percentiles
;; cannot explain what happened in the wall P95 observation.
(def (sample-at-percentile rows key rank)
  (let (ordered
        (list-sort
         (lambda (left right)
           (let ((a (cdr (assq key left))) (b (cdr (assq key right))))
             (if (= a b)
               (< (cdr (assq 'sample left)) (cdr (assq 'sample right)))
               (< a b)))) rows))
    (list-ref ordered (benchmark-percentile-index (length ordered) rank))))

;; Gambit _kernel.scm process-statistics slots 12..19 describe the latest
;; collection in the entire VM, not allocations attributable to this parser.
;; These are snapshots, never differences or sums across collections.
(def (gc-statistics-snapshot statistics)
  (list (cons 'scope 'whole-vm-latest-collection)
        (cons 'gcCount (f64vector-ref statistics 6))
        (cons 'cpuMs (* 1000 (+ (f64vector-ref statistics 12)
                               (f64vector-ref statistics 13))))
        (cons 'wallMs (* 1000 (f64vector-ref statistics 14)))
        (cons 'heapBytes (f64vector-ref statistics 15))
        (cons 'allocatedBytes (f64vector-ref statistics 16))
        (cons 'liveBytes (f64vector-ref statistics 17))
        (cons 'movableBytes (f64vector-ref statistics 18))
        (cons 'stillBytes (f64vector-ref statistics 19))))

(def (sample-gc-snapshot before after)
  ;; A no-GC batch must not inherit the previous batch's collection as its own.
  (and (> (f64vector-ref after 6) (f64vector-ref before 6))
       (gc-statistics-snapshot after)))

;; One warmup and one initial GC per component; timed samples retain naturally
;; occurring GC. Validation and logging are outside the measured batch.
(def (measure-gql-component name samples batch-count thunk expected (calls-per-thunk 1) (allocation-scope 'single-caller))
  (unless (equal? (thunk) expected)
    (error "GQL warmup changed its semantic result" name))
  (##gc)
  (let (baseline (gc-statistics-snapshot (##process-statistics)))
    (write (list 'GQL-GC-BASELINE name baseline)) (newline) (force-output)
    (let loop ((sample 0) (rows '()))
      (if (= sample samples)
        (let (summary
              (list (cons 'stage name) (cons 'sampleCount samples)
                    (cons 'parsesPerSample batch-count)
                    (cons 'wallP50Ms (percentile rows 'wall-ms 50))
                    (cons 'wallP95Ms (percentile rows 'wall-ms 95))
                    (cons 'cpuP50Ms (percentile rows 'cpu-ms 50))
                    (cons 'cpuP95Ms (percentile rows 'cpu-ms 95))
                    (cons 'wallP50Sample (sample-at-percentile rows 'wall-ms 50))
                    (cons 'wallP95Sample (sample-at-percentile rows 'wall-ms 95))
                    (cons 'maxWallSample (sample-at-percentile rows 'wall-ms 100))
                    (cons 'wallP50MsPerParse (/ (percentile rows 'wall-ms 50) batch-count))
                    (cons 'wallP95MsPerParse (/ (percentile rows 'wall-ms 95) batch-count))
                    (cons 'allocationScope allocation-scope)
                    (cons 'gcBaseline baseline)
                    (cons 'allocatedBytesPerParse
                          (and (eq? allocation-scope 'single-caller)
                               (/ (percentile rows 'allocated-bytes 50) batch-count)))
                    (cons 'samples (reverse rows))))
          (write (list 'GQL-STAGE-SUMMARY
                       (filter (lambda (row) (not (eq? (car row) 'samples))) summary))) (newline) (force-output)
          summary)
        (let* ((before (##process-statistics))
               (wall-start (##current-time-point))
               (result
                (let repeat ((remaining (quotient batch-count calls-per-thunk)) (last-result #f))
                  (if (zero? remaining) last-result
                    (repeat (- remaining 1) (thunk)))))
               (wall-ms (* 1000 (- (##current-time-point) wall-start)))
               (after (##process-statistics))
               (delta (lambda (index)
                        (- (f64vector-ref after index) (f64vector-ref before index))))
               (row (list (cons 'sample sample)
                          (cons 'wall-ms wall-ms)
                          (cons 'cpu-ms (* 1000 (+ (delta 0) (delta 1))))
                          ;; Signed observation, not a scheduler attribution. It
                          ;; includes counter/timing noise and may be negative.
                          (cons 'wall-minus-cpu-ms
                                (- wall-ms (* 1000 (+ (delta 0) (delta 1)))))
                          (cons 'gc-count (delta 6))
                          (cons 'gc-wall-ms (* 1000 (delta 5)))
                          (cons 'gc-cpu-ms (* 1000 (+ (delta 3) (delta 4))))
                          (cons 'latest-gc (sample-gc-snapshot before after))
                          (cons 'allocated-bytes (delta 7)))))
          (unless (equal? result expected)
            (error "GQL stage changed its semantic result" name sample))
          (write (list 'GQL-STAGE-SAMPLE name row)) (newline) (force-output)
          (loop (+ sample 1) (cons row rows)))))))

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
        (measure-gql-component 'global-lexing samples batch-count
               (lambda () (lex-source gql-parser source)) lexed)
       (measure-gql-component 'prepared-lr samples batch-count
               (lambda () (call-with-values
                           (lambda () (lr-parse/prepared runtime significant)) list))
               parsed)
       (measure-gql-component 'artifact-publication samples batch-count
               (lambda () (make-success-parse-artifact
                           (parser-machine-grammar-digest gql-parser)
                           source tokens (car parsed)
                           (parser-machine-trivia gql-parser))) reference)
       (measure-gql-component 'full-source samples batch-count
               (lambda () (parse-gql source)) reference)
       )))
