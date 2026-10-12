;;; Upstream engine attribution over an externally admitted real Fold product.
;;; No Orgize API, FFI, Tokio or Python runtime participates.
(import :gerbil-parser/src/runtime/fold-cost
        :gerbil-parser/src/runtime/parse-cost
        :gerbil-parser/benchmarks/real-fold/parse-org-compiled-events)
(export main)
(def (main controls (mode "timed"))
  (unless (member mode '("timed" "counts")) (error "invalid Fold attribution mode" mode))
  (let ((cases (call-with-input-file controls read))
        (session (make-fold-cost-session))
        (events 0))
    (let (before (##process-statistics))
      (parameterize ((current-fold-cost-session session))
        (for-each
         (lambda (control)
           (let (actual (call-with-fold-cost 'whole-fold
                         (lambda () (parse-org-compiled-events (car control) (cadr control)))))
             (unless (equal? actual (caddr control)) (error "real Fold diagnostic mismatch"))
             (set! events (+ events (length actual))))) cases))
      (let* ((after (##process-statistics))
             (delta (lambda (index) (- (f64vector-ref after index) (f64vector-ref before index))))
             (raw (delta 7)) (gc (delta 6)))
        (write (list 'diagnostic-only (cons 'requests (length cases))
                     (cons 'events events) (cons 'cpu-ms (* 1000 (+ (delta 0) (delta 1))))
                     (cons 'gc-count gc) (cons 'allocation-counter-delta raw)
                     (cons 'allocated-bytes (admit-parser-allocation raw gc)))) (newline)))
    (displayln (if (equal? mode "counts")
                 "FIELDS stage calls; leaf timing unavailable (zero placeholders)"
                 "FIELDS stage calls inclusive-us exclusive-instrumented-us failures"))
    (let (rows (fold-cost-rows session))
      (for-each (lambda (row) (write row) (newline))
                (if (equal? mode "counts")
                  (list-sort (lambda (left right) (> (cadr left) (cadr right))) rows) rows)))
    (displayln "DIAGNOSTIC-OK real-fold; NOT PERFORMANCE ACCEPTANCE")))
