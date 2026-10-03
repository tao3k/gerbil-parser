#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Pure Scheme comparison of fresh, one-shot, and cached-LR edits.

(import (only-in :gerbil-parser/languages/arithmetic/v1/parser
                 arithmetic-parser parse-arithmetic-v1)
        (only-in :gerbil-parser/languages/hcl/v2-24/parser
                 hcl-v2-24-parser parse-hcl-v2-24)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-events parse-artifact-valid?)
        (only-in :gerbil-parser/src/runtime/incremental
                 current-lr-fragment-reuse-enabled? apply-edit incremental-session-artifact
                 make-edit make-incremental-session incremental-session-project-artifact
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

;; Topology-changing edits deliberately cross the event-window admission
;; boundary. Each location measures a complete fresh artifact against a cached
;; session update, retaining exact equality and inverse-edit receipts.
(def (median values)
  (let* ((sorted (list-sort < values)) (n (length sorted)) (mid (quotient n 2)))
    (if (odd? n) (list-ref sorted mid)
      (/ (+ (list-ref sorted (- mid 1)) (list-ref sorted mid)) 2.0))))

(def (topology-samples thunk)
  (map (lambda (_)
         (##gc)
         (let* ((started (cpu-time)) (result (thunk))
                (elapsed (* 1000.0 (- (cpu-time) started))))
           (unless result (error "topology benchmark returned no result"))
           elapsed))
       (iota 5)))

(def (measure-topology terms location operation (family 'arithmetic) (capture? #f))
  (let* ((hcl? (eq? family 'hcl-siblings))
         (line "value = 001\n")
         (machine (if hcl? hcl-v2-24-parser arithmetic-parser))
         (parse (if hcl? parse-hcl-v2-24 parse-arithmetic-v1))
         (source (if hcl? (apply string-append (make-list terms line))
                     (string-join (make-list terms "001") " + ")))
         (index (case location ((first) 0) ((middle) (quotient terms 2))
                      (else (- terms 1))))
         (start (if hcl? (* index (string-length line))
                  (if (and (eq? operation 'delete) (= index (- terms 1)))
                    (- (* index 6) 3) (* index 6))))
         (deleted (if (eq? operation 'delete) (if hcl? (string-length line) 6) 0))
         (inserted (if (eq? operation 'insert) (if hcl? "other = 002\n" "002 + ") ""))
         (edit (make-edit start deleted inserted))
         (changed (apply-edit source edit))
         (session (make-incremental-session machine source capture?))
         (fresh (parse changed)))
    (let-values (((next receipt) (parse-incremental-session session edit)))
      (unless (and (parse-artifact-valid? fresh)
                   (equal? fresh (incremental-session-artifact next)))
        (error "topology edit differs from fresh parse" terms location operation))
      (when capture?
        (unless (equal? (incremental-session-project-artifact next) fresh)
          (error "captured topology projection differs" terms location operation)))
      (let* ((restored-text (substring source start (+ start deleted)))
             (inverse (make-edit start (string-length inserted) restored-text)))
        (let-values (((restored ignored) (parse-incremental-session next inverse)))
          (unless (equal? (incremental-session-artifact restored)
                          (incremental-session-artifact session))
            (error "inverse topology edit differs from original" terms location operation))
          (when capture?
            (unless (equal? (incremental-session-project-artifact restored)
                            (incremental-session-artifact session))
              (error "inverse captured projection differs" terms location operation)))))
      (let ((fresh-times (topology-samples (lambda () (parse changed))))
            (cached-times
             (topology-samples
              (lambda ()
                (let-values (((result ignored) (parse-incremental-session session edit)))
                  (incremental-session-artifact result))))))
        (write
         (list (cons 'workload 'significant-token-topology-change)
               (cons 'family family) (cons 'input-units terms) (cons 'location location) (cons 'operation operation)
               (cons 'samples 5) (cons 'recognition-capture? capture?)
               (cons 'complete-artifact-equal? #t) (cons 'inverse-edit-equal? #t)
               (cons 'events (length (parse-artifact-events fresh)))
               (cons 'resumed-shifts (cdr (assq 'resumedSignificantTokenCount receipt)))
               (cons 'remaining-significant-tokens
                     (cdr (assq 'remainingSignificantTokenCount receipt)))
               (cons 'relexed-bytes (cdr (assq 'relexedByteCount receipt)))
               (cons 'converged-suffix-tokens
                     (cdr (assq 'convergedSuffixTokenCount receipt)))
               (cons 'reused-recognition-events
                     (let (entry (assq 'reusedRecognitionEventCount receipt))
                       (if entry (cdr entry) 0)))
               (cons 'fragment-reuse-enabled? (current-lr-fragment-reuse-enabled?))
               (cons 'reused-recognition-fragments
                     (let (entry (assq 'reusedRecognitionFragmentCount receipt))
                       (if entry (cdr entry) 0)))
               (cons 'reused-significant-tokens
                     (let (entry (assq 'reusedSignificantTokenCount receipt))
                       (if entry (cdr entry) 0)))
               (cons 'certificate-probe-bytes
                     (let (entry (assq 'fragmentCertificateProbeByteCount receipt))
                       (if entry (cdr entry) 0)))
               (cons 'fresh-cpu-samples-ms fresh-times)
               (cons 'cached-cpu-samples-ms cached-times)
               (cons 'fresh-cpu-median-ms (median fresh-times))
               (cons 'cached-cpu-median-ms (median cached-times))))
        (newline) (force-output)))))

(def (measure-topology-size terms (family 'arithmetic) (capture? #f))
  (write (list 'topology-size terms 'phase 'semantic-preflight))
  (newline) (force-output)
  (for-each
   (lambda (location)
     (measure-topology terms location 'insert family capture?)
     (measure-topology terms location 'delete family capture?))
   '(first middle last)))

(def (main-default args)
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


(def (main . args)
  (when (and (pair? args) (equal? (car args) "no-reuse"))
    (current-lr-fragment-reuse-enabled? #f)
    (set! args (cdr args)))
  (if (and (pair? args) (member (car args) '("topology" "topology-hcl" "topology-capture" "topology-hcl-capture")))
    (let (sizes (if (null? (cdr args)) '(400 800 1600 3200)
                 (map string->number (cdr args))))
      (unless (every (lambda (size) (and (integer? size) (>= size 2))) sizes)
        (error "topology sizes must be integers of at least two" args))
      (for-each (lambda (size)
                  (measure-topology-size size
                    (if (member (car args) '("topology-hcl" "topology-hcl-capture"))
                      'hcl-siblings 'arithmetic)
                    (if (member (car args) '("topology-capture" "topology-hcl-capture")) #t #f)))
                sizes))
    (main-default args)))

(export main)
