;;; Request-owned region counters versus flat and independently nested frames.
(import (only-in :gerbil-parser/src/runtime/region-scanner
                 prepare-region-plan region-plan-pair-end)
        (only-in :gerbil-parser/src/runtime/parse-cost admit-parser-allocation))
(export benchmark-region-depth)

(def (benchmark-region-depth (samples 11) (iterations 10))
  (unless (and (exact-integer? samples) (positive? samples)
               (exact-integer? iterations) (positive? iterations))
    (error "region depth benchmark requires positive exact counts"))
  (let* ((plan (prepare-region-plan '((";") ((#\" #t ())) (("@{" #\{ #\} 1)) #f)))
         (deep (string-append "@{" (make-string 4096 #\{) "λ" (make-string 4096 #\}) "}"))
         (nested (string-append (string-concatenate (make-list 512 "@{")) "λ" (make-string 512 #\}))))
    (for-each
      (lambda (row)
        (let ((text (car row)) (name (cadr row)))
          (unless (= (region-plan-pair-end plan text 0) (string-length text))
            (error "invalid region reference endpoint" name))
          (let sample ((index 0))
            (when (< index samples)
              (let* ((before (##process-statistics))
                     (sum (let repeat ((count 0) (total 0))
                            (if (= count iterations) total
                              (repeat (+ count 1) (+ total (region-plan-pair-end plan text 0))))))
                     (after (##process-statistics))
                     (delta (lambda (slot) (- (f64vector-ref after slot) (f64vector-ref before slot)))))
                (unless (= sum (* iterations (string-length text)))
                  (error "region endpoint changed" name))
                (write (list 'region-depth name index 'iterations iterations
                             'cpu-ms-per-call (/ (* 1000 (+ (delta 0) (delta 1))) iterations)
                             'allocated-bytes-per-call
                             (let (bytes (admit-parser-allocation (delta 7) (delta 6)))
                               (and bytes (/ bytes iterations)))))
                (newline) (force-output))
              (sample (+ index 1))))))
      (list (list deep 'counter-depth) (list "@{λ}" 'flat) (list nested 'distinct-frames))))
  (displayln "REGION-DEPTH-OK") (force-output))
