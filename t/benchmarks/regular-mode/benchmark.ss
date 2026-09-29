#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Pure Scheme comparison with the previous ASCII-prefiltered mode scan.

(import (only-in :std/vector/vector vector-map/index)
        (only-in :gerbil-parser/src/runtime/scan
                 make-ranked-regular-scanner
                 scan-whitespace scan-horizontal-whitespace scan-newline
                 scan-decimal-digits scan-identifier scan-number-literal))

(def entries
  (list
   (list scan-whitespace char-whitespace? 'white 0 0)
   (list scan-horizontal-whitespace
         (lambda (ch) (or (char=? ch #\space) (char=? ch #\tab)))
         'horizontal 3 1)
   (list scan-newline
         (lambda (ch) (or (char=? ch #\newline) (char=? ch #\return)))
         'newline 2 2)
   (list scan-decimal-digits char-numeric? 'digits 1 3)
   (list scan-identifier
         (lambda (ch) (or (char-alphabetic? ch) (char=? ch #\_)))
         'name 0 4)
   (list scan-number-literal char-numeric? 'number 0 5)))

(def ascii-candidates
  (vector-map/index
   (lambda (index _)
     (filter (lambda (entry) ((cadr entry) (integer->char index)))
             entries))
   (make-vector 128)))

(def combined
  (make-ranked-regular-scanner
   '((whitespace+ white 0 0)
     (horizontal-whitespace+ horizontal 3 1)
     (newline+ newline 2 2)
     (decimal-digit+ digits 1 3)
     (identifier name 0 4)
     (number number 0 5))))

(def (old-mode-scan source offset)
  (let* ((ch (string-ref source offset))
         (candidates
          (if (< (char->integer ch) 128)
            (vector-ref ascii-candidates (char->integer ch))
            entries)))
    (fold
     (lambda (entry best)
       (let (end ((car entry) source offset))
         (if (and end
                  (or (not best)
                      (> end (cadr best))
                      (and (= end (cadr best))
                           (or (> (cadddr entry) (caddr best))
                               (and (= (cadddr entry) (caddr best))
                                    (< (car (cddddr entry))
                                       (cadddr best)))))))
           (list (caddr entry) end (cadddr entry)
                 (car (cddddr entry)))
           best)))
     #f candidates)))

(def (measure label scanner source repetitions)
  (let ((started (##current-time-point)) (last #f))
    (let loop ((remaining repetitions))
      (unless (zero? remaining)
        (set! last (scanner source 0))
        (loop (- remaining 1))))
    (write (list (cons 'scanner label)
                 (cons 'source source)
                 (cons 'repetitions repetitions)
                 (cons 'last last)
                 (cons 'elapsed-total-ms
                       (* 1000.0 (- (##current-time-point) started)))))
    (newline)))

(def (main . args)
  (let (repetitions
        (if (null? args) 50000 (string->number (car args))))
    (for-each
     (lambda (source)
       (unless (equal? (old-mode-scan source 0) (combined source 0))
         (error "regular scanners disagree" source))
       (measure 'old-mode old-mode-scan source repetitions)
       (measure 'combined combined source repetitions))
     '("alpha-123+" "1234567890+" "12.34e+5;"
       "12e+;" " \t \t\n" "αβ-٣+"))))

(export main)
