#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Pure Scheme comparison of fresh, one-shot, and cached-LR edits.

(import (only-in :gerbil-parser/languages/arithmetic/v1/parser
                 arithmetic-parser parse-arithmetic-v1)
        (only-in :gerbil-parser/languages/hcl/v2-24/parser
                 hcl-v2-24-parser parse-hcl-v2-24)
        (only-in :gerbil-parser/src/runtime/incremental
                 apply-edit incremental-session-artifact
                 make-edit make-incremental-session
                 parse-incremental-session parse-source/incremental))

(def (samples thunk)
  (map (lambda (_)
         (let* ((started (##current-time-point))
                (result (thunk))
                (elapsed-ms (* 1000.0 (- (##current-time-point) started))))
           (unless result (error "benchmark returned no result"))
           elapsed-ms))
       (iota 5)))

(def (report label measurements)
  (list label (cons 'best-ms (apply min measurements))
        (list 'samples-ms measurements)))

(def (measure terms)
  (let* ((source (string-join (make-list terms "001") " + "))
         (source-edit (make-edit (* (quotient terms 2) 6) 3 "0002"))
         (changed-source (apply-edit source source-edit))
         (base (parse-arithmetic-v1 source))
         (session (make-incremental-session arithmetic-parser source))
         (fresh (parse-arithmetic-v1 changed-source)))
    (let-values (((one-shot _one-shot-receipt)
                  (parse-source/incremental
                   arithmetic-parser source base source-edit))
                 ((next cached-receipt)
                  (parse-incremental-session session source-edit)))
      (unless (and (equal? one-shot fresh)
                   (equal? (incremental-session-artifact next) fresh))
        (error "incremental benchmark products differ" terms))
      (write
       (list (cons 'terms terms)
             (cons 'relexed-bytes
                   (cdr (assq 'relexedByteCount cached-receipt)))
             (cons 'converged-suffix-tokens
                   (cdr (assq 'convergedSuffixTokenCount cached-receipt)))))
      (newline))
    (write
     (list (cons 'terms terms)
           (cons 'bytes (string-length source))
           (report 'session-build
                   (samples
                    (lambda ()
                      (make-incremental-session arithmetic-parser source))))
           (report 'fresh-edit
                   (samples (lambda ()
                              (parse-arithmetic-v1 changed-source))))
           (report 'one-shot-edit
                   (samples
                    (lambda ()
                      (let-values (((artifact _receipt)
                                    (parse-source/incremental
                                     arithmetic-parser source base source-edit)))
                        artifact))))
           (report 'cached-shifted-event-edit
                   (samples
                    (lambda ()
                      (let-values (((next _receipt)
                                    (parse-incremental-session
                                     session source-edit)))
                        next))))))
    (newline)))

(def (measure-event-reuse terms workload source-edit)
  (let* ((source (string-join (make-list terms "001") " + "))
         (changed-source (apply-edit source source-edit))
         (session (make-incremental-session arithmetic-parser source)))
    (let-values (((next receipt)
                  (parse-incremental-session session source-edit)))
      (unless (equal? (incremental-session-artifact next)
                      (parse-arithmetic-v1 changed-source))
        (error "event reuse benchmark products differ" workload terms))
      (write
       (list (cons 'workload workload)
             (cons 'terms terms)
             (cons 'reused-recognition-events
                   (cdr (assq 'reusedRecognitionEventCount receipt)))
             (report 'fresh-edit
                     (samples
                      (lambda () (parse-arithmetic-v1 changed-source))))
             (report 'cached-event-edit
                     (samples
                      (lambda ()
                        (let-values (((edited _receipt)
                                      (parse-incremental-session
                                       session source-edit)))
                          edited))))))
      (newline))))

(def (measure-window terms)
  (let* ((source (string-join (make-list terms "001") " + "))
         (source-edit
          (make-edit (* (quotient terms 2) 6) 9 "0002 + 0003"))
         (changed-source (apply-edit source source-edit))
         (session (make-incremental-session arithmetic-parser source)))
    (let-values (((next receipt)
                  (parse-incremental-session session source-edit)))
      (unless (equal? (incremental-session-artifact next)
                      (parse-arithmetic-v1 changed-source))
        (error "certified window product differs" terms))
      (write
       (list (cons 'workload 'multi-token-window)
             (cons 'terms terms)
             (cons 'reused-recognition-events
                   (cdr (assq 'reusedRecognitionEventCount receipt)))
             (report 'fresh-edit
                     (samples
                      (lambda () (parse-arithmetic-v1 changed-source))))
             (report 'cached-window-edit
                     (samples
                      (lambda ()
                        (let-values (((edited _receipt)
                                      (parse-incremental-session
                                       session source-edit)))
                          edited))))))
      (newline))))

(def (measure-hcl-window lines workload source-edit)
  (let* ((source (apply string-append
                        (make-list lines "x = 1 /*a*/\n")))
         (changed-source (apply-edit source source-edit))
         (session (make-incremental-session hcl-v2-24-parser source)))
    (let-values (((next receipt)
                  (parse-incremental-session session source-edit)))
      (unless (and (equal? (incremental-session-artifact next)
                           (parse-hcl-v2-24 changed-source))
                   (assq 'reusedRecognitionEventCount receipt))
        (error "certified HCL window product differs" workload lines))
      (write
       (list (cons 'workload workload)
             (cons 'lines lines)
             (cons 'reused-recognition-events
                   (cdr (assq 'reusedRecognitionEventCount receipt)))
             (report 'fresh-edit
                     (samples (lambda ()
                                (parse-hcl-v2-24 changed-source))))
             (report 'cached-window-edit
                     (samples
                      (lambda ()
                        (let-values (((edited _receipt)
                                      (parse-incremental-session
                                       session source-edit)))
                          edited))))))
      (newline))))

(def (measure-trivia-window lines)
  (measure-hcl-window
   lines 'hcl-trivia-token-count-change
   (make-edit (+ (* (quotient lines 2) 12) 6)
              5 "/*a*/ /*b*/")))

(def (measure-mixed-window lines)
  (measure-hcl-window
   lines 'hcl-mixed-token-count-change
   (make-edit (+ (* (quotient lines 2) 12) 4)
              7 "3 /*a*/ /*b*/")))

(def (main . args)
  (for-each measure
            (if (null? args) '(400 800 1600)
                (map string->number args)))
  (for-each
   (lambda (terms)
     (measure-event-reuse
      terms 'same-width-trivia-edit
      (make-edit (- (* (quotient terms 2) 6) 1) 1 "\t"))
     (measure-event-reuse
      terms 'same-width-generic-token-edit
      (make-edit (* (quotient terms 2) 6) 3 "002"))
     (measure-window terms))
   (if (null? args) '(400 800 1600)
       (map string->number args)))
  (measure-trivia-window 100)
  (measure-mixed-window 100))

(export main)
