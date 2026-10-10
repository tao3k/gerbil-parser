;;; Shared CPU/GC/allocation sampling and complete-product diagnostic controls.
(import (only-in :asp-gerbil-scheme/src/benchmark/statistics benchmark-percentile-index)
        (only-in :std/list/list-builder with-list-builder)
        (only-in ../../../src/runtime/parse-cost
                 current-parser-cost-observer admit-parser-allocation)
        (only-in ../../../src/runtime/artifact
                 parse-artifact-valid-for-source? parse-artifact-events parse-artifact-ref))
(export measure-parser-stages measure-artifact-event-storage copy-canonical-event-storage
        measure-parser-component measure-parser-batch measure-parser-cpu-pairs sample-at-percentile
        gc-statistics-snapshot sample-gc-snapshot component-allocation-summary)

(def (require-parser-batch! batch-count calls-per-thunk)
  (unless (and (and (integer? batch-count) (exact? batch-count)) (positive? batch-count)
               (and (integer? calls-per-thunk) (exact? calls-per-thunk)) (positive? calls-per-thunk)
               (zero? (modulo batch-count calls-per-thunk)))
    (error "parser batch requires positive integral calls without truncation")))

;; Admit complete intervals, not raw counter differences across collections.
;; Thread-switching batches remain process observations without caller ownership.
(def (component-allocation-summary rows batch-count allocation-scope)
  (require-parser-batch! batch-count 1)
  (let* ((admitted (if (eq? allocation-scope 'single-caller)
                    (filter (lambda (row) (number? (cdr (assq 'allocated-bytes row)))) rows)
                    '()))
         (per-parse (lambda (rank)
                      (and (pair? admitted)
                           (/ (row-percentile admitted 'allocated-bytes rank) batch-count)))))
    (list (cons 'allocationScope allocation-scope)
          (cons 'allocationSampleCount (length admitted))
          (cons 'allocatedBytesPerParse (per-parse 50))
          (cons 'allocationP95BytesPerParse (per-parse 95)))))

(def (row-percentile rows key rank)
  (percentile (map (lambda (row) (cdr (assq key row))) rows) rank))

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
(def (measure-parser-component name samples batch-count thunk expected (calls-per-thunk 1) (allocation-scope 'single-caller))
  (unless (and (integer? samples) (exact? samples) (positive? samples))
    (error "parser component requires a positive integral sample count" samples))
  (require-parser-batch! batch-count calls-per-thunk)
  (unless (equal? (thunk) expected)
    (error "parser warmup changed its semantic result" name))
  (##gc)
  (let (baseline (gc-statistics-snapshot (##process-statistics)))
    (write (list 'PARSER-GC-BASELINE name baseline)) (newline) (force-output)
    (let loop ((sample 0) (rows '()))
      (if (= sample samples)
        (let (summary
              (append
               (list (cons 'stage name) (cons 'sampleCount samples)
                    (cons 'parsesPerSample batch-count)
                    (cons 'wallP50Ms (row-percentile rows 'wall-ms 50))
                    (cons 'wallP95Ms (row-percentile rows 'wall-ms 95))
                    (cons 'cpuP50Ms (row-percentile rows 'cpu-ms 50))
                    (cons 'cpuP95Ms (row-percentile rows 'cpu-ms 95))
                    (cons 'wallP50Sample (sample-at-percentile rows 'wall-ms 50))
                    (cons 'wallP95Sample (sample-at-percentile rows 'wall-ms 95))
                    (cons 'maxWallSample (sample-at-percentile rows 'wall-ms 100))
                    (cons 'wallP50MsPerParse (/ (row-percentile rows 'wall-ms 50) batch-count))
                    (cons 'wallP95MsPerParse (/ (row-percentile rows 'wall-ms 95) batch-count))
                    (cons 'gcBaseline baseline)
                    (cons 'samples (reverse rows)))
               (component-allocation-summary rows batch-count allocation-scope)))
          (write (list 'PARSER-COMPONENT-SUMMARY
                       (filter (lambda (row) (not (eq? (car row) 'samples))) summary))) (newline) (force-output)
          summary)
        (let (row (measure-parser-batch name sample batch-count thunk expected calls-per-thunk))
          (write (list 'PARSER-COMPONENT-SAMPLE name row)) (newline) (force-output)
          (loop (+ sample 1) (cons row rows)))))))

;;; Shared timed batch primitive. It owns no warmup, forced GC or output, so
;;; matched callers can alternate variants without perturbing GC at each switch.
(def (measure-parser-batch name sample batch-count thunk expected (calls-per-thunk 1))
  (require-parser-batch! batch-count calls-per-thunk)
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
                  (cons 'allocation-counter-delta (delta 7))
                  (cons 'allocated-bytes
                        (admit-parser-allocation (delta 7) (delta 6))))))
  (unless (equal? result expected)
    (error "parser batch changed its semantic result" name sample))
  row))

;;; A representation control, not a parser or a retained-heap measurement.
;;; Reuse payloads exactly as publication does; allocate fresh vectors/list cells.
(def (copy-canonical-event-storage events)
  (with-list-builder (emit!)
    (for-each (lambda (event) (emit! (vector-copy event))) events)))

(def (artifact-with-events artifact events)
  (map (lambda (entry)
         (if (eq? (car entry) 'events) (cons 'events events) entry)) artifact))

(def (measure-artifact-event-storage artifact source (samples 11))
  (unless (and (string? source) (integer? samples) (positive? samples)
               (parse-artifact-valid-for-source? artifact source))
    (error "event storage control requires a source-admitted artifact and positive sample count"))
  (let ((events (parse-artifact-events artifact)) (observations '()))
    (let loop ((sample 0))
      (when (< sample samples)
        (let* ((before (##process-statistics))
               (copy (copy-canonical-event-storage events))
               (after (##process-statistics))
               (delta (lambda (index)
                        (- (f64vector-ref after index) (f64vector-ref before index))))
               (row (list (cons 'cpu-ms (* 1000 (+ (delta 0) (delta 1))))
                          (cons 'allocated-bytes (admit-parser-allocation (delta 7) (delta 6)))
                          (cons 'allocation-counter-delta (delta 7))
                          (cons 'gc-count (delta 6)))))
          ;; Reconstruct and admit the entire product outside the copy interval.
          (let (product (artifact-with-events artifact copy))
            (unless (and (equal? product artifact)
                         (parse-artifact-valid-for-source? product source))
              (error "event storage control changed the complete admitted product" sample)))
          (set! observations (cons row observations))
          (write (list 'artifact-event-storage-sample sample row))
          (newline) (force-output))
        (loop (+ sample 1))))
    (let* ((allocations (filter number? (map (lambda (row) (cdr (assq 'allocated-bytes row))) observations)))
           (summary
            (list (cons 'samples samples) (cons 'events (length events))
                  (cons 'vector-slots (foldl (lambda (event n) (+ n (vector-length event))) 0 events))
                  (cons 'cpu-p50-ms (median (map (lambda (row) (cdr (assq 'cpu-ms row))) observations)))
                  (cons 'cpu-p95-ms (percentile (map (lambda (row) (cdr (assq 'cpu-ms row))) observations) 95))
                  (cons 'allocation-p50-bytes (median allocations))
                  (cons 'allocation-p95-bytes (percentile allocations 95))
                  (cons 'allocation-samples (length allocations)))))
      (write (list 'artifact-event-storage-summary summary)) (newline) (force-output)
      summary)))

(def (median values)
  (let* ((ordered (list-sort < values)) (n (length ordered)) (middle (quotient n 2)))
    (and (positive? n)
         (if (odd? n) (list-ref ordered middle)
           (/ (+ (list-ref ordered (- middle 1)) (list-ref ordered middle)) 2.0)))))

(def (percentile values rank)
  (and (pair? values)
       (let (ordered (list-sort < values))
         (list-ref ordered (benchmark-percentile-index (length ordered) rank)))))

;;; Observer overhead is included; these samples are not latency comparisons.
(def (measure-parser-stages parser source (samples 11))
  (unless (and (procedure? parser) (string? source)
               (integer? samples) (positive? samples))
    (error "stage diagnostics require a parser, source and positive sample count"))
  (let ((reference (parser source)) (observations '()))
    (unless (parse-artifact-valid-for-source? reference source)
      (error "stage reference failed source-bound admission"))
    (let loop ((sample 0))
      (when (< sample samples)
        (let* ((stages '())
               (before (##process-statistics))
               (artifact
                (parameterize ((current-parser-cost-observer
                                (lambda (stage receipt)
                                  (set! stages (cons (cons stage receipt) stages)))))
                  (parser source)))
               (after (##process-statistics))
               (delta (lambda (index)
                        (- (f64vector-ref after index) (f64vector-ref before index))))
               (whole
                (list (cons 'cpu-ms (* 1000 (+ (delta 0) (delta 1))))
                      (cons 'allocated-bytes (admit-parser-allocation (delta 7) (delta 6)))
                      (cons 'allocation-counter-delta (delta 7))
                      (cons 'gc-count (delta 6)) (cons 'completed #t)))
               (rows (cons (cons 'observed-complete-request whole) (reverse stages))))
          ;; Full equality and source admission are outside the measured request.
          (unless (and (equal? reference artifact)
                       (parse-artifact-valid-for-source? artifact source))
            (error "stage instrumentation changed the complete admitted product" sample))
          (set! observations (append rows observations))
          (write (list 'parser-stage-sample sample rows)) (newline) (force-output))
        (loop (+ sample 1))))
    (let (summary
          (map (lambda (stage)
                 (let* ((receipts (map cdr (filter (lambda (row) (eq? stage (car row))) observations)))
                        (allocations (filter number? (map (lambda (r) (cdr (assq 'allocated-bytes r))) receipts)))
                        (completed (foldl (lambda (r total)
                                            (if (cdr (assq 'completed r)) (+ total 1) total))
                                          0 receipts)))
                   (list (cons 'stage stage) (cons 'request-samples samples)
                         (cons 'samples (length receipts))
                         (cons 'completed-samples completed)
                         (cons 'incomplete-samples (- (length receipts) completed))
                         (cons 'cpu-p50-ms (median (map (lambda (r) (cdr (assq 'cpu-ms r))) receipts)))
                         (cons 'cpu-p95-ms
                               (percentile (map (lambda (r) (cdr (assq 'cpu-ms r))) receipts) 95))
                         (cons 'allocation-p50-bytes (median allocations))
                         (cons 'allocation-p95-bytes (percentile allocations 95))
                         (cons 'allocation-samples (length allocations)))))
               (foldl (lambda (row stages)
                        (if (memq (car row) stages) stages (cons (car row) stages)))
                      '() observations)))
      (write (list 'parser-stage-summary 'input-characters (string-length source)
                   'events (length (parse-artifact-events reference))
                   'source-digest (parse-artifact-ref reference 'sourceDigest)
                   'grammar-digest (parse-artifact-ref reference 'grammarDigest) summary))
      (newline) (force-output)
      (displayln "PARSER-STAGE-COST-OK samples=" samples) (force-output)
      summary)))

;;; Same-process CPU comparison with complete products and alternating order.
(def (cpu-pair-field row key) (cdr (assq key row)))
(def (measure-parser-cpu-pairs name phase groups calls expected left right)
         (unless (and (exact-integer? groups) (>= groups 20))
    (error "paired CPU proof requires at least twenty groups" groups))
  (require-parser-batch! calls 1)
  (unless (and (equal? (left) expected) (equal? (right) expected))
    (error "paired CPU warmup differs from complete expected product" name))
  (##gc)
         (let loop ((group 0) (rows '()))
           (if (= group groups)
             (map (lambda (rank) (cpu-pair-field (sample-at-percentile rows 'ratio rank) 'ratio)) '(10 50 90))
             (let* ((order (if (even? group) '(left right right left) '(right left left right)))
                    (batches (map (lambda (variant)
                      (cons variant (measure-parser-batch (list 'literal-edges name phase) group calls
                                      (if (eq? variant 'left) left right) expected))) order))
                    (sum (lambda (variant)
                      (apply + (map (lambda (batch) (cpu-pair-field (cdr batch) 'cpu-ms))
                                    (filter (lambda (batch) (eq? (car batch) variant)) batches)))))
                    (cpu (sum 'left)))
               (unless (and (positive? cpu)
                            (andmap (lambda (batch) (>= (cpu-pair-field (cdr batch) 'cpu-ms) 1.0)) batches))
                 (error "edge CPU batch does not resolve one millisecond" name phase group))
               (let (row (list (cons 'sample group) (cons 'ratio (/ (sum 'right) cpu))))
                 (write (list 'PARSER-CPU-PAIR name phase row 'batches batches)) (newline) (force-output)
                 (loop (+ group 1) (cons row rows)))))))

