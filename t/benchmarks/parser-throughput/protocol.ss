;;; Engine-owned qualification protocol; adapters supply a compiled parser.
(import (only-in :gerbil-parser/src/runtime/parse-cost admit-parser-allocation)
        (only-in :std/os/time current-system-time-precise
                 timespec-tv_sec timespec-tv_nsec))
(export parser-throughput-percentile parser-throughput-gate
        parser-throughput-batch run-parser-throughput)

(def loads '(1000 10000 100000))
(def minimum-throughput 100000)

;; Use the installed SDK's precise OS clock, not floating-point epoch seconds
;; whose subtraction quantizes sub-microsecond requests to zero. This is a
;; realtime clock: reject backwards movement; do not call it monotonic.
(def (clock-ns)
  (let (time (current-system-time-precise))
    (+ (* (timespec-tv_sec time) 1000000000) (timespec-tv_nsec time))))

(def (parser-throughput-percentile sorted percent)
  (unless (and (vector? sorted) (positive? (vector-length sorted))
               (exact-integer? percent) (<= 1 percent 100))
    (error "invalid nearest-rank percentile input" percent))
  (vector-ref sorted
    (- (quotient (+ (* percent (vector-length sorted)) 99) 100) 1)))

(def (parser-throughput-gate completed wall-s)
  (and (exact-integer? completed) (positive? completed)
       (real? wall-s) (> wall-s 0)
       (<= wall-s (/ completed minimum-throughput))))

(def (parser-throughput-batch parser inputs expected)
  (let* ((count (vector-length inputs))
         (latencies (make-vector count 0))
         (unresolved 0)
         (stats (##process-statistics))
         (begin (clock-ns)))
    (unless (and (> count 0) (= count (vector-length expected)))
      (error "benchmark input/oracle extent mismatch"))
    (let loop ((index 0))
      (unless (= index count)
        (let* ((start (clock-ns))
               (result (parser (vector-ref inputs index)))
               (elapsed (- (clock-ns) start)))
          (when (< elapsed 0) (error "benchmark clock moved backwards"))
          ;; A zero interval is below this clock's observable resolution,
          ;; never proof of a zero-cost parser call.
          (when (= elapsed 0) (set! unresolved (+ unresolved 1)))
          (vector-set! latencies index (/ elapsed 1000.0))
          ;; Full semantic validation is outside request timing, inside the
          ;; conservative batch wall. Never label this as bare Fold cost.
          (unless (equal? result (vector-ref expected index))
            (error "benchmark semantic mismatch" index))
          (loop (+ index 1)))))
    (let* ((wall (/ (- (clock-ns) begin) 1000000000.0))
           (after (##process-statistics))
           (delta (lambda (slot) (- (f64vector-ref after slot)
                                   (f64vector-ref stats slot))))
           (sorted (list->vector (list-sort < (vector->list latencies)))))
      (unless (> wall 0) (error "invalid batch wall clock" wall))
      `((completed . ,count) (batch-wall-s . ,wall)
        (throughput-requests/s . ,(/ count wall))
        (request-p50-us . ,(parser-throughput-percentile sorted 50))
        (request-p95-us . ,(parser-throughput-percentile sorted 95))
        (request-p99-us . ,(parser-throughput-percentile sorted 99))
        (request-max-us . ,(vector-ref sorted (- count 1)))
        (sub-resolution-request-samples . ,unresolved)
        (process-cpu-s . ,(+ (delta 0) (delta 1)))
        (gc-count . ,(delta 6))
        (allocation-counter-delta . ,(delta 7))
        (allocated-bytes . ,(admit-parser-allocation (delta 7) (delta 6)))
        (throughput-wall-gate . ,(parser-throughput-gate count wall))))))

(def (run-parser-throughput label identity parser oracle input mode)
  (unless (and (string? label) (not (equal? label ""))
               (string? identity) (not (equal? identity ""))
               (memq mode '(preflight full)))
    (error "benchmark requires source identity and preflight/full mode"))
  (def (run-load count batches phase)
    ;; Generate changing inputs and independent expected values before timing.
    ;; Both stay retained: memory evidence includes benchmark oracle storage.
    (let loop ((batch 1))
      (when (<= batch batches)
       (let* ((inputs (list->vector
                       (map (lambda (index) (input (+ index (* count batch))))
                            (iota count))))
              (expected (list->vector (map oracle (vector->list inputs)))))
          (unless (equal? (parser (vector-ref inputs 0)) (vector-ref expected 0))
            (error "parser warmup semantic mismatch"))
          (let (receipt (parser-throughput-batch parser inputs expected))
            (write `((schema . parser-throughput.v1) (label . ,label)
                     (source-identity . ,identity) (phase . ,phase)
                     (requests . ,count) (batch . ,batch)
                     (input-index-base . ,(* count batch))
                     (minimum-throughput-requests/s . ,minimum-throughput)
                     (maximum-batch-wall-s . ,(/ count minimum-throughput))
                     (request-boundary . compiled-parser-call)
                     (batch-boundary . parser-calls-plus-complete-semantic-comparison)
                     (percentile-method . nearest-rank-per-request)
                     (clock . sdk-clock-realtime-integer-nanoseconds)
                     (tail-latency-gate . not-configured)
                     (memory-boundary . process-allocation-counter-and-gc-not-rss)
                     (input-and-oracle-retained . #t)
                     (samples . ,receipt)))
            (newline) (force-output)
            (unless (cdr (assq 'throughput-wall-gate receipt))
              (error "PERFORMANCE-GATE-FAILED; stop rejected candidate" count batch)))
          (loop (+ batch 1))))))
  ;; A failing preflight is a rejection, never a truncated successful matrix.
  (run-load 1000 1 'preflight)
  (if (eq? mode 'full)
    (begin
      (for-each (lambda (count) (run-load count 20 'acceptance)) loads)
      (displayln "PARSER-THROUGHPUT-FULL-OK loads=3 batches-per-load=20"))
    (displayln "PARSER-THROUGHPUT-PREFLIGHT-OK; full acceptance not run"))
  (force-output))
