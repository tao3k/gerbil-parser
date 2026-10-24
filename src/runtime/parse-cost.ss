;;; Optional request-local diagnostics; disabled calls do not allocate a thunk.
(export current-parser-cost-observer with-parser-cost-stage admit-parser-allocation)
;; The SDK allocation counter includes occupied still objects and may fall at GC.
;; Retain the raw delta separately; admit an allocation comparison only without GC.
(def (admit-parser-allocation delta collections)
  (and (zero? collections) (>= delta 0) delta))
(def current-parser-cost-observer (make-parameter #f))
(def (call-with-parser-cost-stage observer name thunk)
  (let ((before (##process-statistics))
        (started (##current-time-point))
        (completed? #f))
    (dynamic-wind void
      (lambda ()
        (let (result (call-with-values thunk list))
          (set! completed? #t)
          (apply values result)))
      (lambda ()
        (let* ((elapsed (* 1000 (- (##current-time-point) started)))
               (after (##process-statistics))
               (delta (lambda (index) (- (f64vector-ref after index) (f64vector-ref before index)))))
          (observer name
            (list (cons 'wall-ms elapsed)
                  (cons 'cpu-ms (* 1000 (+ (delta 0) (delta 1))))
                  (cons 'allocated-bytes (admit-parser-allocation (delta 7) (delta 6)))
                  (cons 'allocation-counter-delta (delta 7))
                  (cons 'gc-count (delta 6))
                  (cons 'completed completed?))))))))
(defrules with-parser-cost-stage ()
  ((_ name expression)
   (let (observer (current-parser-cost-observer))
     (if observer
       (call-with-parser-cost-stage observer name (lambda () expression))
       expression))))
