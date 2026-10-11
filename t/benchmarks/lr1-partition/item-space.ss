#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Complete compiler products, not a request-latency proxy. Each observation
;;; checks the entire LRSpec outside the measured construction interval.
(import (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/compiler/lr-automaton make-item-layout make-core-symbol-catalog)
        (only-in :gerbil-parser/src/compiler/lr lower-rules production-table production-rhs production-terminal-catalog)
        (only-in :gerbil-parser/src/runtime/parse-cost admit-parser-allocation)
        (only-in :gerbil-parser/t/fixtures/lr1-construction skew-width-rules))
(export main observe-lr-construction observe-lr-layout)

;;; Separate the newly added inverse-owner preparation from total compilation.
;;; Retain the last product and consume its symbols outside the timed interval.
;;; The same source entry runs against original and current native producers.
(def (observe-lr-layout workload rules root (samples 20) (calls 2000))
  (unless (and (exact-integer? samples) (positive? samples)
               (exact-integer? calls) (positive? calls))
    (error "layout observation requires positive integral samples and calls"))
  (let* ((productions (lower-rules rules root)) (table (production-table productions)))
    (let-values (((terminals _index) (production-terminal-catalog productions)))
      (for-each
       (lambda (catalog?)
         (def (prepare)
           (let (layout (make-item-layout table terminals))
             (if catalog? (make-core-symbol-catalog table layout) layout)))
         (def (project product)
           (filter values (vector->list
                           (if catalog? product (make-core-symbol-catalog table product)))))
         (let (expected (project (prepare)))
           (let sample ((index 0))
             (when (< index samples)
               (let* ((before (##process-statistics)) (started (cpu-time))
                      (last (let repeat ((remaining calls) (last #f))
                              (if (zero? remaining) last
                                (repeat (- remaining 1) (prepare)))))
                      (cpu-ms (* 1000.0 (- (cpu-time) started)))
                      (after (##process-statistics)))
                 (unless (equal? (project last) expected)
                   (error "layout preparation changed symbols" workload catalog? index))
                 (write (list 'ITEM-LAYOUT-SAMPLE workload catalog? index 'calls calls
                              'cpu-ms-per-call (/ cpu-ms calls)
                              'allocated-bytes-per-call
                              (let (bytes (admit-parser-allocation
                                           (- (##f64vector-ref after 7) (##f64vector-ref before 7))
                                           (- (##f64vector-ref after 6) (##f64vector-ref before 6))))
                                (and bytes (/ bytes calls)))))
                 (newline) (force-output))
               (sample (+ index 1))))))
       '(#f #t)))))

(def (observe-lr-construction workload rules root route samples (calls 1))
  (unless (and (exact-integer? calls) (positive? calls))
    (error "construction observation requires positive integral calls" calls))
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
                 (found (let repeat ((remaining calls) (last #f))
                          (if (zero? remaining) last
                            (repeat (- remaining 1) (compile)))))
                 (cpu-ms (* 1000.0 (- (cpu-time) started)))
                 (after (##process-statistics)))
            (unless (equal? reference found)
              (error "complete compiler product changed" workload route sample))
            (write (list 'ITEM-SPACE-SAMPLE workload route sample
                         'positions positions 'padded-positions padded 'cpu-ms (/ cpu-ms calls)
                         'allocated-bytes
                         (let (bytes (admit-parser-allocation
                                      (- (##f64vector-ref after 7) (##f64vector-ref before 7))
                                      (- (##f64vector-ref after 6) (##f64vector-ref before 6))))
                           (and bytes (/ bytes calls)))
                         'in-call-gcs (- (##f64vector-ref after 6) (##f64vector-ref before 6))
                         'calls calls))
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
