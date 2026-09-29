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

(def (main . args)
  (let* ((terms (if (null? args) 3200 (string->number (car args))))
         (samples (if (or (null? args) (null? (cdr args)))
                    30 (string->number (cadr args)))))
    (unless (and (integer? terms) (positive? terms)
                 (integer? samples) (positive? samples))
      (error "expected positive terms and sample count" args))
    (let (source (addition-source terms))
      (write (list (cons 'terms terms)
                   (cons 'source-characters (string-length source))))
      (newline)
      (let loop ((n 0))
        (when (< n (+ samples 3))
          (##gc)
          (let* ((started (##current-time-point))
                 (artifact (parse-arithmetic-v1 source))
                 (ms (* 1000.0 (- (##current-time-point) started))))
            (unless (and (parse-artifact-success? artifact)
                         (equal? (parse-artifact-roundtrip artifact) source))
              (error "long arithmetic parse failed" terms))
            (when (>= n 3)
              (write (list (cons 'sample (- n 3)) (cons 'elapsed-ms ms)))
              (newline)))
          (loop (+ n 1)))))))

(export main)
