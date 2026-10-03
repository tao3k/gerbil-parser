#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Repeatable long-input ParseArtifact comparison across package revisions.

(import (only-in :gerbil-parser/languages/arithmetic/v1/parser
                 parse-arithmetic-v1)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success? parse-artifact-roundtrip))

(def (addition-source terms)
  (let (source (make-string (- (* 2 terms) 1) #\1))
    (let loop ((offset 1))
      (when (< offset (string-length source))
        (string-set! source offset #\+)
        (loop (+ offset 2))))
    source))

(def (addition-lines-source lines)
  (let (source (make-string (- (* 3 lines) 2) #\1))
    (let loop ((offset 0))
      (when (< (+ offset 2) (string-length source))
        (string-set! source (+ offset 1) #\+)
        (string-set! source (+ offset 2) #\newline)
        (loop (+ offset 3))))
    source))

(def (main . args)
  (let* ((terms (if (null? args) 3200 (string->number (car args))))
         (samples (if (or (null? args) (null? (cdr args)))
                    30 (string->number (cadr args))))
         (shape (if (and (pair? args) (pair? (cdr args))
                         (pair? (cddr args)))
                  (string->symbol (caddr args)) 'terms)))
    (unless (and (integer? terms) (positive? terms)
                 (integer? samples) (positive? samples)
                 (memq shape '(terms lines)))
      (error "expected positive size, sample count, and terms/lines shape"
             args))
    (let (source (if (eq? shape 'lines)
                  (addition-lines-source terms)
                  (addition-source terms)))
      (write (list (cons 'input-shape shape)
                   (cons (if (eq? shape 'lines) 'lines 'terms) terms)
                   (cons 'source-characters (string-length source))))
      (newline)
      (let loop ((n 0))
        (when (< n (+ samples 3))
          (##gc)
          ;; Keep process CPU beside elapsed time: a stalled host must not
          ;; be mistaken for extra parser work.
          (let* ((started (##current-time-point))
                 (cpu-started (cpu-time))
                 (artifact (parse-arithmetic-v1 source))
                 (cpu-ms (* 1000.0 (- (cpu-time) cpu-started)))
                 (ms (* 1000.0 (- (##current-time-point) started))))
            (unless (and (parse-artifact-success? artifact)
                         (equal? (parse-artifact-roundtrip artifact) source))
              (error "long arithmetic parse failed" terms))
            (when (>= n 3)
              (write (list (cons 'sample (- n 3))
                           (cons 'elapsed-ms ms)
                           (cons 'cpu-ms cpu-ms)))
              (newline)))
          (loop (+ n 1)))))))

(export main)
