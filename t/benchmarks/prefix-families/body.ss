;;; Native callers, complete event parity and request-cost boundaries.
(import (only-in :gerbil-parser/src/runtime/parse-cost admit-parser-allocation))
(export main)

(def routes (list (cons 'rich-linear rich-linear) (cons 'rich-fused rich-fused)
                  (cons 'stress-linear stress-linear) (cons 'stress-fused stress-fused)))

(def (document index)
  (string-append "#+TITLE: " (number->string index) "\r\n#+description: λ中🦀\n"
                 "#+KEY1: value\r#+key31: é\r\n\n#+CAPTION:"))

(def (positive-count text)
  (let (count (string->number text))
    (unless (and (exact-integer? count) (positive? count)) (error "invalid benchmark count" text))
    count))

(def (measure documents route)
  (let* ((before (##process-statistics)) (start (##current-time-point))
         (count (foldl (lambda (text sum) (+ sum (length ((cdr route) text)))) 0 documents))
         (wall (* 1000 (- (##current-time-point) start))) (after (##process-statistics))
         (delta (lambda (slot) (- (f64vector-ref after slot) (f64vector-ref before slot)))))
    (unless (= count (* 20 (length documents))) (error "native prefix fold lost events" route count))
    (list 'wall-ms wall 'cpu-ms (* 1000 (+ (delta 0) (delta 1))) 'gc-count (delta 6)
          'allocation-counter-delta (delta 7) 'allocated-bytes (admit-parser-allocation (delta 7) (delta 6)))))

(def (main (samples-text "8") . sizes-text)
  (for-each (lambda (check) (check))
            (list rich-linear-oracle rich-fused-oracle stress-linear-oracle stress-fused-oracle))
  (displayln "PREFIX-INTERPRETER-OK") (force-output)
  (let ((samples (positive-count samples-text))
        (sizes (map positive-count (if (null? sizes-text) '("1000" "2000" "10000") sizes-text))))
    (for-each
     (lambda (size)
       (let (documents (map document (iota size)))
         (for-each
          (lambda (text)
            (unless (and (equal? (rich-linear text) (rich-fused text))
                         (equal? (stress-linear text) (stress-fused text)))
              (error "native prefix family changed complete events" text)))
          (append '("" "#+T" "#+TITLE:" "#+TITLE:x" "#+DESCRIPTIO:" "é中🦀" "#+KEY31:\n") documents))
         (displayln "PREFIX-PARITY-OK documents=" size) (force-output)
         (for-each
          (lambda (sample)
            (let (offset (modulo sample 4))
              (for-each
               (lambda (route)
                 (write (append (list 'prefix-family 'documents size 'sample sample 'mode (car route))
                                (measure documents route))) (newline) (force-output))
               (append (drop routes offset) (take routes offset))))) (iota samples)))) sizes))
  (displayln "PREFIX-BENCHMARK-OK") (force-output))
