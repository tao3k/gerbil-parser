#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Paired CPU samples for the bounded canonical trial in follow construction.

(import (only-in :gerbil-parser/src/compiler/lr lower-rules lr-spec-ref
                 production-table)
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/t/fixtures/lr1-construction
                 lr1-context-family-rules mixed-context-family-rules
                 acyclic-mixed-context-family-rules
                 unreachable-repeated-context-family-rules))

(def (main . args)
  (let* ((contexts (if (pair? args) (string->number (car args)) 48))
         (samples (if (and (pair? args) (pair? (cdr args)))
                    (string->number (cadr args)) 20))
         (shape (if (and (pair? args) (pair? (cdr args))
                         (pair? (cddr args)))
                  (string->symbol (caddr args)) 'independent)))
    (unless (and (integer? contexts) (positive? contexts)
                 (integer? samples) (positive? samples)
                 (memq shape '(independent mixed acyclic-mixed unreachable-repeated)))
      (error "expected positive context and sample counts, then independent/mixed/acyclic-mixed/unreachable-repeated"
             args))
    (let* ((rules ((case shape
                     ((mixed) mixed-context-family-rules)
                     ((acyclic-mixed) acyclic-mixed-context-family-rules)
                     ((unreachable-repeated)
                      unreachable-repeated-context-family-rules)
                     (else lr1-context-family-rules))
                   contexts))
           (productions (vector-length
                         (production-table (lower-rules rules 'source-file)))))
      (def (measure construction)
        (##gc)
        (let* ((started (cpu-time))
               (spec (compile-lr-spec rules 'source-file 'reject #f
                                      construction)))
          (vector (* 1000.0 (- (cpu-time) started))
                  (lr-spec-ref spec 'state-count)
                  (or (lr-spec-ref spec 'follow-block-count) 0)
                  (lr-spec-ref spec 'output-item-count))))
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
            (unless (if (memq shape '(mixed acyclic-mixed))
                      (and (< (vector-ref follow 1)
                              (vector-ref canonical 1))
                           (< (vector-ref follow 2)
                              (vector-ref follow 3)))
                      (= (vector-ref canonical 1) (vector-ref follow 1)))
              (error "construction topology differs" shape contexts sample))
            (write (list (cons 'shape shape)
                         (cons 'contexts contexts)
                         (cons 'productions productions)
                         (cons 'sample sample)
                         (cons 'canonical-states (vector-ref canonical 1))
                         (cons 'follow-states (vector-ref follow 1))
                         (cons 'canonical-cpu-ms (vector-ref canonical 0))
                         (cons 'follow-cpu-ms (vector-ref follow 0))
                         (cons 'follow-block-count (vector-ref follow 2))
                         (cons 'follow-items (vector-ref follow 3))))
            (newline))
          (loop (+ sample 1)))))))

(export main)
