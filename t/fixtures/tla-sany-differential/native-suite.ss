#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; One runtime shares imported dependencies; worker-local harnesses own test state.
(import :gerbil/expander
        (only-in :std/test/base TestSuite? TestModule TestHarness TestConfig test-run! test-result-ok?))
(load "t/fixtures/tla-sany-differential/preload.ss")
(prefer-native-interfaces!)

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
  ;; Native FFI dependencies must be admitted before evaluating source suites;
  ;; the interpreter cannot evaluate their C declarations. This shared loader
  ;; cache survives every owner in the single runtime.
  (preload-test-imports file)
  (let* ((context (import-test-owner file))
         (exports (module-context-export context))
         (suites '()) (setup void) (cleanup void))
    (for-each
      (lambda (exported)
        (when (zero? (module-export-phi exported))
          (let* ((name (module-export-name exported))
                 (binding (core-resolve-module-export exported)))
            (cond
              ((eq? name 'test-setup!) (set! setup (eval (binding-id binding))))
              ((eq? name 'test-cleanup!) (set! cleanup (eval (binding-id binding))))
              ((string-suffix? "-test" (symbol->string name))
               (let (suite (eval (binding-id binding)))
                 (unless (TestSuite? suite) (error "invalid exported test suite" file name))
                 (set! suites (cons suite suites)))))))) exports)
    (when (null? suites) (error "native test module exports no suites" file))
    (TestHarness file (TestConfig verbosity: 6 capture-output?: #f)
                 (list (TestModule file (reverse suites) '() setup cleanup)))))

(defstruct native-test-owner (file harness preparation-seconds))

(def (call-with-owner-budget file emit thunk (budget 90))
  (let-values (((input output) (open-string-pipe '(buffering: #f))))
    (let* ((started (##current-time-point))
           (worker
             (spawn
               (lambda ()
                 (parameterize ((current-output-port output) (current-error-port output))
                   (try
                    (with-catch
                      (lambda (e) (display-exception e) (cons 'error e))
                      (lambda () (cons 'value (thunk))))
                    (finally (close-output-port output)))))))
           (cases 0))
      (try
       (let loop ()
         (let* ((reader (spawn (lambda () (read-line input))))
                (line (thread-join! reader
                        (max 0.001 (min 5 (- budget (- (##current-time-point) started)))) 'quiet)))
           (cond
             ((or (eq? line 'quiet) (> (- (##current-time-point) started) budget))
              (emit "MODULE-BATCH-TIMEOUT " file " reason="
                    (if (>= (- (##current-time-point) started) budget) 'total-budget 'idle-timeout))
              ;; Fail the runtime, never resume a timed-out thread's partial state.
              (exit 70))
             ((eof-object? line) (void))
             (else
              (when (string-prefix? "CASE-OK " line) (set! cases (+ cases 1)))
              (emit "MODULE-OUTPUT " file " " line)
              (loop)))))
       (let (result (thread-join! worker))
         (if (eq? (car result) 'error) (raise (cdr result))
           (values (cdr result) cases)))
       (finally (close-input-port input))))))

(def (run-test-owner owner emit)
  (let (file (native-test-owner-file owner))
    (emit "MODULE-BATCH-START " file)
    (let-values (((ok? cases)
                   (call-with-owner-budget file emit
                     (lambda () (test-result-ok? (test-run! (native-test-owner-harness owner))))
                     (- 90 (native-test-owner-preparation-seconds owner)))))
      (unless ok? (error "native test owner failed" file))
      (when (zero? cases) (error "native test module ran no cases" file))
      (emit "MODULE-BATCH-OK " file " cases=" cases))))

(def (main . roots)
  (let* ((files (apply append (map test-files roots)))
         (capacity (string->number (getenv "GERBIL_TEST_CORES" "1")))
         (lock (make-mutex)) (pending '()) (failures '()))
    (unless (= (length files) (length (unique-test-files files)))
      (error "native test suite has duplicate module owners"))
    (when (null? files) (error "native test suite has no modules" roots))
    (unless (and (integer? capacity) (exact? capacity) (> capacity 0))
      (error "GERBIL_TEST_CORES must be a positive integer"))
    (def (locked thunk)
      (mutex-lock! lock)
      (try (thunk) (finally (mutex-unlock! lock))))
    (def (emit . values)
      (locked (lambda () (apply displayln values) (force-output))))
    (def (next-owner)
      (locked (lambda ()
                (and (pair? pending)
                     (let (file (car pending))
                       (set! pending (cdr pending)) file)))))
    (def (worker)
      (let loop ()
        (let (owner (next-owner))
          (when owner
            ;; Stop admitting new jobs after failure and join every started
            ;; Worker. Never lose an exception in thread-join!.
            (with-catch
              (lambda (e)
                (locked (lambda () (set! failures (cons (cons (native-test-owner-file owner) e) failures)) (set! pending '())))
                (emit "MODULE-BATCH-FAILED " (native-test-owner-file owner)))
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
                 (make-native-test-owner file harness (- (##current-time-point) started))))) files))
    (emit "NATIVE-SUITE-WORKERS " (min capacity (length files)))
    (let (workers (map (lambda (_) (spawn worker)) (iota (min capacity (length files)))))
      (for-each thread-join! workers))
    (unless (null? failures) (raise (cdar failures)))
    (emit "NATIVE-SUITE-OK modules=" (length files))
    (emit "OK")))
