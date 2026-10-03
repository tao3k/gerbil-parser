#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Pure Scheme scan-cost comparison for a fixed 64-rule literal mode.

(import (only-in :gerbil-parser/src/runtime/scan
                 make-literal-end-scanner make-ranked-literal-scanner))

(def entries
  (map (lambda (index)
         (list (string-append "keyword-" (number->string index))
               (string->symbol
                (string-append "Keyword" (number->string index)))
               0 index))
       (iota 64)))

(def separate-scanners
  (map (lambda (entry)
         (cons entry (make-literal-end-scanner (list (car entry)))))
       entries))

(def combined-scanner (make-ranked-literal-scanner entries))

(def (scan-separately source)
  (let loop ((remaining separate-scanners) (selected #f))
    (if (null? remaining)
      selected
      (let* ((rule (car remaining))
             (entry (car rule))
             (end ((cdr rule) source 0)))
        (loop (cdr remaining)
              (if (and end
                       (or (not selected) (> end (cadr selected))))
                (list (cadr entry) end (caddr entry) (cadddr entry))
                selected))))))

(def (measure label source scanner repetitions)
  (let ((started (##current-time-point))
        (last #f))
    (let loop ((remaining repetitions))
      (unless (zero? remaining)
        (set! last (scanner source))
        (loop (- remaining 1))))
    (write
     (list (cons 'scanner label)
           (cons 'source source)
           (cons 'repetitions repetitions)
           (cons 'last last)
           (cons 'elapsed-total-ms
                 (* 1000.0 (- (##current-time-point) started)))))
    (newline)))

(def (main . args)
  (let (repetitions
        (if (null? args) 10000 (string->number (car args))))
    (unless (and (integer? repetitions) (> repetitions 0))
      (error "expected a positive repetition count" args))
    (for-each
     (lambda (source)
       (unless (equal? (scan-separately source)
                       (combined-scanner source 0))
         (error "literal scanners disagree" source))
       (measure 'separate source scan-separately repetitions)
       (measure 'combined source
                (lambda (text) (combined-scanner text 0)) repetitions))
     '("keyword-63!" "not-a-keyword"))))

(export main)
