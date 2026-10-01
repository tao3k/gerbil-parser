#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Paired CPU samples for the bounded canonical trial in follow construction.

(import (only-in :gerbil-parser/src/compiler/lr lower-rules lr-spec-ref
                 production-table)
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/t/fixtures/lr1-construction
                 lr1-context-family-rules))

(def (main . args)
  (let* ((contexts (if (pair? args) (string->number (car args)) 48))
         (samples (if (and (pair? args) (pair? (cdr args)))
                    (string->number (cadr args)) 20)))
    (unless (and (integer? contexts) (positive? contexts)
                 (integer? samples) (positive? samples))
      (error "expected positive context and sample counts" args))
    (let* ((rules (lr1-context-family-rules contexts))
           (productions (vector-length
                         (production-table (lower-rules rules 'source-file)))))
      (def (measure construction)
        (##gc)
        (let* ((started (cpu-time))
               (spec (compile-lr-spec rules 'source-file 'reject #f
                                      construction)))
          (vector (* 1000.0 (- (cpu-time) started))
                  (lr-spec-ref spec 'state-count)
                  (or (lr-spec-ref spec 'follow-block-count) 0))))
      ;; Warm both routes before recording paired, alternating batches.
      (measure 'canonical-lr1)
      (measure 'follow-partition-lr1)
      (let loop ((sample 0))
        (when (< sample samples)
          (let* ((first (measure (if (odd? sample)
                                   'follow-partition-lr1 'canonical-lr1)))
                 (second (measure (if (odd? sample)
                                    'canonical-lr1 'follow-partition-lr1)))
                 (canonical (if (odd? sample) second first))
                 (follow (if (odd? sample) first second)))
            (unless (= (vector-ref canonical 1) (vector-ref follow 1))
              (error "construction state counts differ" contexts sample))
            (write (list (cons 'contexts contexts)
                         (cons 'productions productions)
                         (cons 'sample sample)
                         (cons 'states (vector-ref canonical 1))
                         (cons 'canonical-cpu-ms (vector-ref canonical 0))
                         (cons 'follow-cpu-ms (vector-ref follow 0))
                         (cons 'follow-block-count (vector-ref follow 2))))
            (newline))
          (loop (+ sample 1)))))))

(export main)
