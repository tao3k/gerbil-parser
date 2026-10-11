#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Complete compiler products, not a request-latency proxy. Each observation
;;; checks the entire LRSpec outside the measured construction interval.
(import (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/compiler/lr lower-rules production-table production-rhs)
        (only-in :gerbil-parser/src/runtime/parse-cost admit-parser-allocation)
        (only-in :gerbil-parser/t/fixtures/lr1-construction skew-width-rules))
(export main observe-lr-construction)

(def (observe-lr-construction workload rules root route samples)
  (let* ((table (production-table (lower-rules rules root)))
         (positions (foldl (lambda (production count)
                             (+ count 1 (length (production-rhs production))))
                           0 (vector->list table)))
         (padded (* (vector-length table)
                    (foldl (lambda (production width)
                             (max width (+ 1 (length (production-rhs production)))))
                           0 (vector->list table)))))
    (def (compile) (compile-lr-spec rules root 'selective-glr #f route))
    (let (reference (compile))
      (write (list 'ITEM-SPACE-PRODUCT workload route reference))
      (newline) (force-output)
      (let loop ((sample 0))
        (when (< sample samples)
          (let* ((before (##process-statistics))
                 (started (cpu-time))
                 (found (compile))
                 (cpu-ms (* 1000.0 (- (cpu-time) started)))
                 (after (##process-statistics)))
            (unless (equal? reference found)
              (error "complete compiler product changed" workload route sample))
            (write (list 'ITEM-SPACE-SAMPLE workload route sample
                         'positions positions 'padded-positions padded 'cpu-ms cpu-ms
                         'allocated-bytes
                         (admit-parser-allocation
                          (- (##f64vector-ref after 7) (##f64vector-ref before 7))
                          (- (##f64vector-ref after 6) (##f64vector-ref before 6)))
                         'in-call-gcs (- (##f64vector-ref after 6) (##f64vector-ref before 6))))
            (newline) (force-output))
          (loop (+ sample 1)))))))

(def (main . args)
  (let (samples (if (pair? args) (string->number (car args)) 10))
    (unless (and (<= (length args) 1) (exact-integer? samples) (positive? samples))
      (error "expected one positive sample count" args))
    (for-each
     (lambda (size)
       (for-each (lambda (route)
                   (observe-lr-construction size (skew-width-rules (car size) (cadr size))
                                            'source-file route samples))
                 '(lalr canonical-lr1 follow-partition-lr1)))
     '((32 16) (128 64) (512 256)))
    (displayln "ITEM-SPACE-BENCHMARK-OK") (force-output)))
