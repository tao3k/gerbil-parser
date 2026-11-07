;;; Upstream engine attribution over an externally admitted real Fold product.
;;; No Orgize API, FFI, Tokio or Python runtime participates.
(import :gerbil-parser/src/runtime/fold-cost
        :gerbil-parser/src/runtime/parse-cost
        :gerbil-parser/benchmarks/real-fold/parse-org-compiled-events)
(export main)
(def (main controls (mode "timed") (expected-digest #f))
  (unless (member mode '("timed" "counts" "prepared-counts"))
    (error "invalid Fold attribution mode" mode))
  (let* ((input (call-with-input-file controls read))
         (prepared? (equal? mode "prepared-counts"))
         (count-only? (member mode '("counts" "prepared-counts")))
         (cases
          (if prepared?
            (begin
              (unless (and (list? input) (= (length input) 4)
                           (eq? (car input) 'real-fold-preflight.v1)
                           ;; Diagnostic products intentionally reject the
                           ;; production throughput binder. Bind the corpus
                           ;; here; receipts separately seal generated IR and
                           ;; binary bytes. This never admits performance.
                           (string? expected-digest) (equal? (cadr input) expected-digest)
                           (vector? (caddr input)) (vector? (cadddr input))
                           (= (vector-length (caddr input)) 1000)
                           (= (vector-length (cadddr input)) 1000))
                (error "invalid independent attribution corpus"))
              (map (lambda (index)
                     (list (vector-ref (caddr input) index) '()
                           (vector-ref (cadddr input) index))) (iota 1000))) input))
        (session (make-fold-cost-session))
        (events 0))
    (let (before (##process-statistics))
      (parameterize ((current-fold-cost-session session))
        (for-each
         (lambda (control)
           (let (actual (call-with-fold-cost 'whole-fold
                         (lambda () (parse-org-compiled-events (car control) (cadr control)))))
             (unless (equal? actual (caddr control)) (error "real Fold diagnostic mismatch"))
             (set! events (+ events (if (u8vector? actual) (u8vector-length actual) (length actual)))))) cases))
      (let* ((after (##process-statistics))
             (delta (lambda (index) (- (f64vector-ref after index) (f64vector-ref before index))))
             (raw (delta 7)) (gc (delta 6)))
        (write (list 'diagnostic-only (cons 'requests (length cases))
                     (cons 'events events) (cons 'cpu-ms (* 1000 (+ (delta 0) (delta 1))))
                     (cons 'gc-count gc) (cons 'allocation-counter-delta raw)
                     (cons 'allocated-bytes (admit-parser-allocation raw gc)))) (newline)))
    (displayln (if count-only?
                 "FIELDS stage count; ordinary-run-scanned-bytes and summary-byte-visits use bytes, other ordinary-run stages use counts; leaf timing unavailable (zero placeholders)"
                 "FIELDS stage calls inclusive-us exclusive-instrumented-us failures"))
    (let (rows (fold-cost-rows session))
      (for-each (lambda (row) (write row) (newline))
                (if count-only?
                  (list-sort (lambda (left right) (> (cadr left) (cadr right))) rows) rows)))
    (when prepared?
      (write (list 'diagnostic-corpus-identity expected-digest)) (newline))
    (displayln "DIAGNOSTIC-OK real-fold; NOT PERFORMANCE ACCEPTANCE")))
