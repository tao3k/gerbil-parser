#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; One runtime shares imported dependencies; worker-local harnesses own test state.
(import :gerbil/expander
        (only-in :std/test/base TestSuite? TestModule TestHarness TestConfig test-run! test-result-ok?))
(load "t/fixtures/tla-sany-differential/preload.ss")
(prefer-compiled-interfaces!)
(import (only-in :clan/poo/object .o)
        (only-in :core/observability/testing-case
                 poo-flow-default-testing-case-profile
                 poo-flow-current-testing-case-profile))

;; The Case sampler measures the shared process heap, including every loaded
;; Suite. Align its absolute cap with the full runner budget; retain its growth,
;; sampling, failure, and duration policies through POO inheritance.
(def testing-case-profile
  (.o (:: @ poo-flow-default-testing-case-profile)
      (heap-limit-bytes (* 3 1024 1024 1024))))

(def (import-test-owner file)
  (let (importer (current-expander-module-import))
    (parameterize
      ((current-expander-module-import
         (lambda (path reload?)
           (displayln "INTERFACE-IMPORT " path) (force-output)
           (let (context (importer path reload?))
             (displayln "INTERFACE-IMPORTED " path) (force-output)
             context))))
      (import-module file #f #t))))

(def (test-files directory)
  (if (not (eq? (file-type directory) 'directory))
    (list directory)
    (apply append
   (map (lambda (name)
          (if (string-prefix? "." name) '()
            (let (path (path-expand name directory))
              (cond ((eq? (file-type path) 'directory) (test-files path))
                    ((string-suffix? "-test.ss" name) (list path))
                    (else '())))))
        (list-sort string<? (directory-files directory))))))

(def (unique-test-files files)
  (let (seen (make-table test: equal?))
    (filter (lambda (file)
              (let (path (path-normalize (path-expand file)))
                (if (table-ref seen path #f) #f
                  (begin (table-set! seen path #t) #t)))) files)))

(def (prepare-test-owner file)
  ;; Only this admission section owns expander/module initialization. Public
  ;; expander exports and std/test objects are the normal gxtest suite protocol.
  ;; C FFI dependencies must be admitted before evaluating source suites;
  ;; the interpreter cannot evaluate their C declarations. This shared loader
  ;; cache survives every owner in the single runtime.
  (preload-test-imports file)
  (let* ((context (import-test-owner file))
         (exports (module-context-export context))
         (suites '()) (setup void) (cleanup void) (affinity #f))
    (for-each
      (lambda (exported)
        (when (zero? (module-export-phi exported))
          (let* ((name (module-export-name exported))
                 (binding (core-resolve-module-export exported)))
            (cond
              ((eq? name 'test-worker-affinity)
               (set! affinity (eval (binding-id binding)))
               (unless (symbol? affinity) (error "invalid test Worker affinity" file affinity)))
              ((eq? name 'test-setup!) (set! setup (eval (binding-id binding))))
              ((eq? name 'test-cleanup!) (set! cleanup (eval (binding-id binding))))
              ((string-suffix? "-test" (symbol->string name))
               (let (suite (eval (binding-id binding)))
                 (unless (TestSuite? suite) (error "invalid exported test suite" file name))
                 (set! suites (cons suite suites)))))))) exports)
    (when (null? suites) (error "Scheme test module exports no suites" file))
    (cons (TestHarness file (TestConfig verbosity: 6 capture-output?: #f)
                       (list (TestModule file (reverse suites) '() setup cleanup)))
          affinity)))

(defstruct test-owner (file harness preparation-seconds affinity))

(def (call-with-owner-budget file emit thunk (budget 90))
  (let-values (((input output) (open-string-pipe '(buffering: #f))))
    (let* ((started (##current-time-point))
           ;; The caller is the persistent Worker. Only output supervision runs
           ;; elsewhere, preserving C ABI runtime thread ownership.
           (monitor
             (spawn
               (lambda ()
                 (let loop ((cases 0))
                   (let* ((reader (spawn (lambda () (read-line input))))
                          (line (thread-join! reader
                                  (max 0.001 (min 5 (- budget (- (##current-time-point) started)))) 'quiet)))
                     (cond
                       ((or (eq? line 'quiet) (> (- (##current-time-point) started) budget))
                        (emit "MODULE-BATCH-TIMEOUT " file " reason="
                              (if (>= (- (##current-time-point) started) budget) 'total-budget 'idle-timeout))
                        (exit 70))
                       ((eof-object? line) cases)
                       (else
                        (emit "MODULE-OUTPUT " file " " line)
                        (loop (+ cases (if (string-prefix? "CASE-OK " line) 1 0)))))))))))
      (try
       (let (result
              (parameterize ((current-output-port output) (current-error-port output))
                (try
                 (with-catch
                   (lambda (e) (display-exception e) (cons 'error e))
                   (lambda () (cons 'value (thunk))))
                 (finally (close-output-port output)))))
         (let (cases (thread-join! monitor))
           (if (eq? (car result) 'error) (raise (cdr result))
             (values (cdr result) cases))))
       (finally (close-input-port input))))))

(def (run-test-owner owner emit)
  (let (file (test-owner-file owner))
    (emit "MODULE-BATCH-START " file)
    (let-values (((ok? cases)
                   (call-with-owner-budget file emit
                     (lambda ()
                       (parameterize ((poo-flow-current-testing-case-profile
                                       testing-case-profile))
                         (test-result-ok? (test-run! (test-owner-harness owner)))))
                     (- 90 (test-owner-preparation-seconds owner)))))
      (unless ok? (error "Scheme test owner failed" file))
      (when (zero? cases) (error "Scheme test module ran no cases" file))
      (emit "MODULE-BATCH-OK " file " cases=" cases))))

(def (main . roots)
  (let* ((files (apply append (map test-files roots)))
         (capacity (string->number (getenv "GERBIL_TEST_CORES" "1")))
         (lock (make-mutex)) (pending '()) (failures '()))
    (unless (= (length files) (length (unique-test-files files)))
      (error "Scheme test suite has duplicate module owners"))
    (when (null? files) (error "Scheme test suite has no modules" roots))
    (unless (and (integer? capacity) (exact? capacity) (> capacity 0))
      (error "GERBIL_TEST_CORES must be a positive integer"))
    (def (locked thunk)
      (mutex-lock! lock)
      (try (thunk) (finally (mutex-unlock! lock))))
    (def (emit . values)
      (locked (lambda () (apply displayln values) (force-output))))
    (def (next-owner index)
      (locked
        (lambda ()
          (let loop ((remaining pending) (skipped '()))
            (cond
              ((null? remaining) #f)
              ((or (zero? index) (not (test-owner-affinity (car remaining))))
               (set! pending (append (reverse skipped) (cdr remaining)))
               (car remaining))
              (else (loop (cdr remaining) (cons (car remaining) skipped))))))))
    ;; Declared runtime-affine owners share Worker zero. Other modules remain
    ;; work-stealing jobs; no filename or language-specific scheduler rules.
    (def (worker index)
      (let loop ()
        (let (owner (next-owner index))
          (when owner
            ;; Stop admitting new jobs after failure and join every started
            ;; Worker. Never lose an exception in thread-join!.
            (with-catch
              (lambda (e)
                (locked (lambda () (set! failures (cons (cons (test-owner-file owner) e) failures)) (set! pending '())))
                (emit "MODULE-BATCH-FAILED " (test-owner-file owner)))
              (lambda () (run-test-owner owner emit)))
            (loop)))))
    ;; Dynamic module admission completes before any test Worker runs. The
    ;; runtime loader/expander is a shared preparation owner, not a worker job.
    (set! pending
      (map (lambda (file)
             (emit "MODULE-PREPARE-START " file)
             (let (started (##current-time-point))
               (let-values (((harness _)
                              (call-with-owner-budget file emit
                                (lambda () (prepare-test-owner file)))))
                 (emit "MODULE-PREPARE-OK " file)
                 (make-test-owner file (car harness) (- (##current-time-point) started) (cdr harness))))) files))
    (emit "TEST-SUITE-WORKERS " (min capacity (length files)))
    (let (workers (map (lambda (index) (spawn (lambda () (worker index)))) (iota (min capacity (length files)))))
      (for-each thread-join! workers))
    (unless (null? failures) (raise (cdar failures)))
    (emit "TEST-SUITE-OK modules=" (length files))
    (emit "OK")))
