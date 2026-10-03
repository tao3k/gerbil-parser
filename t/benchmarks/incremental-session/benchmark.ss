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
                 current-lr-probe-reuse-enabled? current-lr-fragment-reuse-enabled? current-lr-source-index-enabled? current-lr-lexical-plan-reuse-enabled? incremental-session-source-index incremental-session-source-modes apply-edit incremental-session-artifact
                 make-edit make-incremental-session incremental-session-project-artifact incremental-session-recognition-root
                 parse-incremental-session parse-source/incremental))

(import (only-in :gerbil-parser/src/runtime/lr-parser
                 lr-recognition-fragment? lr-recognition-fragment-children
                 lr-recognition-view? lr-recognition-view-base))

(import (only-in :gerbil-parser/src/runtime/source-index source-index-count source-index-height source-index-audit source-index->lists))

(import (only-in :gerbil-parser/src/compiler/machine
                 current-lexical-plan-sharing-enabled? parser-machine-prepare-lexer parser-machine-lexical-plans))
(def (plan-count plans)
  (length (foldl (lambda (plan found) (if (memq plan found) found (cons plan found))) '() (vector->list plans))))
(def (measure-lexical-preparation)
  (for-each
   (lambda (row)
     (let* ((machine (cdr row)) (plans (parser-machine-lexical-plans machine))
            (times (topology-samples
                     (lambda ()
                       (let-values (((lexer prepared) (parser-machine-prepare-lexer machine))) prepared)) 'lexical-preparation)))
       (let-values (((lexer prepared) (parser-machine-prepare-lexer machine)))
         (write (list (cons 'workload 'lexical-plan-preparation)
                      (cons 'family (car row)) (cons 'samples +topology-sample-count+)
                      (cons 'plan-sharing-enabled? (current-lexical-plan-sharing-enabled?))
                      (cons 'lr-modes (vector-length plans)) (cons 'prepared-plans (plan-count prepared))
                      (cons 'cpu-samples-ms times) (cons 'cpu-median-ms (median times))))
         (newline) (force-output))))
   (list (cons 'arithmetic arithmetic-parser) (cons 'hcl hcl-v2-24-parser))))

(def (audit-source-index session)
  (when (and (current-lr-source-index-enabled?) (incremental-session-source-modes session))
    (let (tree (incremental-session-source-index session))
      (unless (source-index-audit tree) (error "source index measures differ"))
      (let-values (((tokens modes) (source-index->lists tree)))
        (unless (equal? modes (incremental-session-source-modes session))
          (error "source index modes differ from captured parser stream"))))))

(def (grammar-fragment-count session)
  (let loop ((pending (list (incremental-session-recognition-root session))) (count 0))
    (if (null? pending) count
      (let (piece (car pending))
        (cond ((lr-recognition-view? piece)
               (loop (cons (lr-recognition-view-base piece) (cdr pending)) count))
              ((lr-recognition-fragment? piece)
               (loop (append (lr-recognition-fragment-children piece) (cdr pending)) (+ count 1)))
              (else (loop (cdr pending) count)))))))

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

(def +topology-sample-count+
  (let (count (string->number (or (getenv "GERBIL_PARSER_BENCHMARK_SAMPLES" #f) "5")))
    (unless (and (integer? count) (<= 1 count 100))
      (error "topology sample count must be an integer from 1 to 100"))
    count))

(def (topology-samples thunk (phase 'unspecified))
  (map (lambda (index)
         (##gc)
         (let* ((started (cpu-time)) (result (thunk))
                (elapsed (* 1000.0 (- (cpu-time) started))))
           (unless result (error "topology benchmark returned no result"))
           ;; Real sample completion, outside the measured call; no heartbeat.
           (write (list 'topology-sample phase (+ index 1) 'cpu-ms elapsed))
           (newline) (force-output)
           elapsed))
       (iota +topology-sample-count+)))

(def (measure-topology terms location operation (family 'arithmetic) (capture? #f))
  (let* ((hcl? (memq family '(hcl-siblings hcl-nested-siblings)))
         (nested? (eq? family 'hcl-nested-siblings))
         (line (if nested?
                 (string-append "group {\n"
                   (apply string-append (make-list 80 "value = 001\n")) "}\n")
                 "value = 001\n"))
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
         (inserted (if (eq? operation 'insert) (if hcl? (if nested? line "other = 002\n") "002 + ") ""))
         (edit (make-edit start deleted inserted))
         (changed (apply-edit source edit))
         (session (make-incremental-session machine source capture?))
         (forest-count (grammar-fragment-count session))
         (fresh (parse changed)))
    (let-values (((next receipt) (parse-incremental-session session edit)))
      (unless (and (parse-artifact-valid? fresh)
                   (equal? fresh (incremental-session-artifact next)))
        (error "topology edit differs from fresh parse" terms location operation))
      (when capture?
        (audit-source-index next)
        (unless (equal? (incremental-session-project-artifact next) fresh)
          (error "captured topology projection differs" terms location operation)))
      (let* ((restored-text (substring source start (+ start deleted)))
             (inverse (make-edit start (string-length inserted) restored-text)))
        (let-values (((restored ignored) (parse-incremental-session next inverse)))
          (unless (equal? (incremental-session-artifact restored)
                          (incremental-session-artifact session))
            (error "inverse topology edit differs from original" terms location operation))
          (when capture?
            (audit-source-index restored)
            (unless (equal? (incremental-session-project-artifact restored)
                            (incremental-session-artifact session))
              (error "inverse captured projection differs" terms location operation)))))
      (let ((fresh-times (topology-samples (lambda () (parse changed)) 'fresh))
            (cached-times
             (topology-samples
              (lambda ()
                (let-values (((result ignored) (parse-incremental-session session edit)))
                  (incremental-session-artifact result))) 'cached)))
        (write
         (list (cons 'workload 'significant-token-topology-change)
               (cons 'family family) (cons 'input-units terms) (cons 'location location) (cons 'operation operation)
               (cons 'samples +topology-sample-count+) (cons 'recognition-capture? capture?)
               (cons 'complete-artifact-equal? #t) (cons 'inverse-edit-equal? #t)
               (cons 'events (length (parse-artifact-events fresh)))
               (cons 'initial-grammar-fragments forest-count)
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
               (cons 'probe-reuse-enabled? (current-lr-probe-reuse-enabled?))
               (cons 'lexical-plan-reuse-enabled? (current-lr-lexical-plan-reuse-enabled?))
               (cons 'source-index-enabled? (current-lr-source-index-enabled?))
               (cons 'source-index-token-count (source-index-count (incremental-session-source-index session)))
               (cons 'source-index-height (source-index-height (incremental-session-source-index next)))
               (cons 'source-index-fresh-tokens (let (entry (assq 'sourceIndexFreshTokenCount receipt)) (and entry (cdr entry))))
               (cons 'source-index-shared-tokens (let (entry (assq 'sourceIndexSharedTokenCount receipt)) (and entry (cdr entry))))
               (cons 'source-index-new-mode-chunks (let (entry (assq 'sourceIndexNewModeChunkCount receipt)) (and entry (cdr entry))))
               (cons 'source-index-new-chunks (let (entry (assq 'sourceIndexNewChunkCount receipt)) (and entry (cdr entry))))
               (cons 'reused-recognition-fragments
                     (let (entry (assq 'reusedRecognitionFragmentCount receipt))
                       (if entry (cdr entry) 0)))
               (cons 'reused-significant-tokens
                     (let (entry (assq 'reusedSignificantTokenCount receipt))
                       (if entry (cdr entry) 0)))
               (cons 'certificate-probe-bytes
                     (let (entry (assq 'fragmentCertificateProbeByteCount receipt))
                       (if entry (cdr entry) 0)))
               (cons 'reused-probe-tokens
                     (let (entry (assq 'fragmentProbeReuseTokenCount receipt))
                       (if entry (cdr entry) 0)))
               (cons 'reused-probe-bytes
                     (let (entry (assq 'fragmentProbeReuseByteCount receipt))
                       (if entry (cdr entry) 0)))
               (cons 'cursor-visited-frames
                     (let (entry (assq 'fragmentCursorVisitCount receipt))
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

(def (measure-history units nested?)
  (let* ((line (if nested? (string-append "group {\n" (apply string-append (make-list 80 "value = 001\n")) "}\n") "value = 001\n"))
         (inserted (if nested? line "other = 002\n"))
         (width (string-length inserted)) (mid (* (quotient units 2) (string-length line)))
         (source (apply string-append (make-list units line)))
         (session (make-incremental-session hcl-v2-24-parser source #t))
         (edits (list (make-edit 0 0 inserted) (make-edit (+ mid width) 0 inserted)
                      (make-edit 0 width "") (make-edit mid width "")
                      (make-edit (string-length source) 0 inserted)
                      (make-edit (string-length source) width ""))))
    (write (list 'history-size units 'nested? nested? 'phase 'semantic-preflight)) (newline) (force-output)
    (let validate ((rest edits) (current session) (text source))
      (unless (null? rest)
        (let-values (((next receipt) (parse-incremental-session current (car rest))))
          (let* ((changed (apply-edit text (car rest))) (fresh (parse-hcl-v2-24 changed)))
            (unless (and (equal? fresh (incremental-session-artifact next))
                         (equal? fresh (incremental-session-project-artifact next)))
              (error "history artifact/projector differs" units (length rest)))
            (audit-source-index next)
            (validate (cdr rest) next changed)))))
    (let (times (topology-samples
                (lambda ()
                  (let loop ((rest edits) (current session))
                    (if (null? rest) (incremental-session-artifact current)
                      (let-values (((next receipt) (parse-incremental-session current (car rest))))
                        (loop (cdr rest) next))))) 'history))
      (write (list (cons 'workload 'sequential-source-history)
                   (cons 'family (if nested? 'hcl-nested-siblings 'hcl-siblings))
                   (cons 'input-units units) (cons 'edits 6)
                   (cons 'samples +topology-sample-count+)
                   (cons 'lexical-plan-reuse-enabled? (current-lr-lexical-plan-reuse-enabled?))
               (cons 'source-index-enabled? (current-lr-source-index-enabled?))
                   (cons 'complete-artifact-equal? #t) (cons 'inverse-edit-equal? #t)
                   (cons 'cached-cpu-samples-ms times) (cons 'cached-cpu-median-ms (median times))))
      (newline) (force-output))))

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
  (when (and (pair? args) (equal? (car args) "no-plan-sharing"))
    (current-lexical-plan-sharing-enabled? #f)
    (set! args (cdr args)))
  (when (and (pair? args) (equal? (car args) "no-plan-reuse"))
    (current-lr-lexical-plan-reuse-enabled? #f)
    (set! args (cdr args)))
  (when (and (pair? args) (equal? (car args) "lexical-plan-prepare"))
    (measure-lexical-preparation)
    (exit 0))
  (when (and (pair? args) (equal? (car args) "source-index"))
    (current-lr-source-index-enabled? #t)
    (set! args (cdr args)))
  (when (and (pair? args) (equal? (car args) "no-source-index"))
    (current-lr-source-index-enabled? #f)
    (set! args (cdr args)))
  (when (and (pair? args) (equal? (car args) "no-probe-reuse"))
    (current-lr-probe-reuse-enabled? #f)
    (set! args (cdr args)))
  (when (and (pair? args) (equal? (car args) "no-reuse"))
    (current-lr-fragment-reuse-enabled? #f)
    (set! args (cdr args)))
  (if (and (pair? args) (member (car args) '("history-hcl-capture" "history-hcl-nested-capture")))
    (for-each (lambda (size) (measure-history size (equal? (car args) "history-hcl-nested-capture")))
              (map string->number (cdr args)))
  (if (and (pair? args) (member (car args) '("topology" "topology-hcl" "topology-capture" "topology-hcl-capture" "topology-hcl-nested-capture")))
    (let (sizes (if (null? (cdr args)) '(400 800 1600 3200)
                 (map string->number (cdr args))))
      (unless (every (lambda (size) (and (integer? size) (>= size 2))) sizes)
        (error "topology sizes must be integers of at least two" args))
      (for-each (lambda (size)
                  (measure-topology-size size
                    (cond ((equal? (car args) "topology-hcl-nested-capture") 'hcl-nested-siblings)
                          ((member (car args) '("topology-hcl" "topology-hcl-capture")) 'hcl-siblings)
                          (else 'arithmetic))
                    (if (member (car args) '("topology-capture" "topology-hcl-capture" "topology-hcl-nested-capture")) #t #f)))
                sizes))
    (main-default args))))

(export main)
