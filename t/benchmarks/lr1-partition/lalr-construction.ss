#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Complete default LALR construction for the real TLA+ layout grammar.
;;; CPU timing excludes imports, explicit GC, and complete LRSpec comparison.

(import (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/compiler/lr
                 lower-rules production-table lr-spec-ref)
        (only-in :gerbil-parser/src/compiler/parser-ir parser-ir-ref)
        (only-in :gerbil-parser/languages/tla-plus/grammars/layout
                 tla-plus-layout-parser-ir))

(def (median values)
  (let* ((sorted (list-sort < values))
         (count (length sorted))
         (middle (quotient count 2)))
    (if (odd? count)
      (list-ref sorted middle)
      (/ (+ (list-ref sorted (- middle 1))
            (list-ref sorted middle)) 2.0))))

(def (main . args)
  (let ((samples (if (pair? args) (string->number (car args)) 20)))
    (unless (and (<= (length args) 1)
                 (integer? samples) (positive? samples))
      (error "expected one optional positive sample count" args))
    (write '(workload tla-layout construction lalr phase warmup))
    (newline) (force-output)
    (let* ((rules (parser-ir-ref tla-plus-layout-parser-ir 'rules))
           (productions (vector-length
                         (production-table (lower-rules rules 'source-file)))))
      ;; Omit the construction argument to exercise the production default.
      (def (compile) (compile-lr-spec rules 'source-file 'selective-glr))
      (let (reference (compile))
        (let loop ((sample 0) (times '()) (gc-counts '()) (gc-times '()))
          (if (= sample samples)
            (begin
              (write (list 'summary 'workload 'tla-layout 'construction 'lalr
                           'samples samples 'rules (length rules)
                           'productions productions
                           'states (lr-spec-ref reference 'state-count)
                           'cpu-median-ms (median times)
                           'in-call-gcs-median (median gc-counts)
                           'in-call-gc-cpu-median-ms (median gc-times)))
              (newline) (force-output))
            (begin
              (##gc)
              (let* ((before (##process-statistics))
                     (started (cpu-time))
                     (spec (compile))
                     (elapsed (* 1000.0 (- (cpu-time) started)))
                     (after (##process-statistics))
                     (gc-count (- (##f64vector-ref after 6) (##f64vector-ref before 6)))
                     (gc-ms (* 1000.0
                               (+ (- (##f64vector-ref after 3) (##f64vector-ref before 3))
                                  (- (##f64vector-ref after 4) (##f64vector-ref before 4))))))
                (unless (equal? reference spec)
                  (error "complete LRSpec changed between samples" sample))
                (write (list 'sample sample 'workload 'tla-layout
                             'construction 'lalr 'cpu-ms elapsed
                             'in-call-gcs gc-count 'in-call-gc-cpu-ms gc-ms))
                (newline) (force-output)
                (loop (+ sample 1) (cons elapsed times)
                      (cons gc-count gc-counts) (cons gc-ms gc-times))))))))))

(export main)
