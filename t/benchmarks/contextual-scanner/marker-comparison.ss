#!/usr/bin/env gxi
;;; Matched SDK component controls; only copied/guarded use ABBA order.
;;; Does not admit a complete-parser or separately compiled native speedup.
(import (only-in :std/string/misc string=))
(export main)
(def (copied source marker start end)
  (string=? (substring source start end) marker))
(def (ranged source marker start end)
  (and (string= source marker start end) #t))
(def (guarded source marker start end)
  (and (= (- end start) (string-length marker))
       (string=? (substring source start end) marker)))
(def (observe label compare source marker start end expected)
  (##gc)
  (let* ((before (##process-statistics)) (cpu (cpu-time))
         (matches (let loop ((n 1000) (count 0))
                    (if (zero? n) count
                      (loop (- n 1) (+ count (if (compare source marker start end) 1 0))))))
         (cpu-ms (* 1000 (- (cpu-time) cpu)))
         (after (##process-statistics)))
    (unless (= matches (if expected 1000 0)) (error "marker comparison mismatch" label matches))
    (write (list label 'cpu-ms cpu-ms
                 'gc-count (- (f64vector-ref after 6) (f64vector-ref before 6))
                 'allocation-counter-delta (- (f64vector-ref after 7) (f64vector-ref before 7))))
    (newline) (force-output)))
(def (main . _args)
(for-each
 (lambda (entry)
   (let* ((name (car entry)) (content (cadr entry)) (marker (caddr entry))
          (expected (cadddr entry)) (source (string-append "prefix" content "suffix")))
     (displayln "SDK-MARKER-COMPONENT " name)
     (for-each
      (lambda (method) (observe (car method) (cdr method) source marker 6
                                (+ 6 (string-length content)) expected))
      (list (cons 'copied copied) (cons 'guarded guarded)
            (cons 'guarded guarded) (cons 'copied copied)
            (cons 'ranged ranged) (cons 'ranged ranged)))))
 (list (list 'long-body (make-string 1024 (integer->char 955)) "EOF" #f)
       (list 'equal-long (make-string 1024 (integer->char 955))
                         (make-string 1024 (integer->char 955)) #t)
       (list 'equal-short "EOF" "EOF" #t)
       (list 'empty "" "" #t)
       (list 'same-size-mismatch "EOX" "EOF" #f)))
(displayln "SDK-MARKER-COMPONENT-OK"))
