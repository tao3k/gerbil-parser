;;; -*- Gerbil -*-
;;; One runtime shares imported dependencies; worker-local harnesses own test state.
(displayln "SUITE-IMPORT expander-and-test-base") (force-output)
(import :gerbil/expander
        (only-in :std/sync/barrier Barrier? make-barrier barrier-post! barrier-wait!)
        (only-in :std/test/base TestSuite? TestModule TestHarness TestConfig test-run! test-result-ok?))
(displayln "SUITE-IMPORTED expander-and-test-base") (force-output)
(load "scripts/test-plan.ss")
;; Source-first callers still need the prepared native FFI module before the
;; public import. The interpreter cannot evaluate its C declarations.
(preload-module "gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process")
(def native-test-exit!
  (eval '(begin
           (import (only-in :gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process
                            test-child-process-exit!))
           test-child-process-exit!)))
(displayln "SUITE-IMPORT case-profile") (force-output)
(import (only-in :clan/poo/object .o)
        (only-in :core/observability/testing-case
                 poo-flow-default-testing-case-profile
                 poo-flow-current-testing-case-profile))

(displayln "SUITE-IMPORTED case-profile") (force-output)

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

(defstruct test-owner (file harness preparation-seconds affinity exclusive?))

(def (prepare-test-owner file (covered-names '()) (seen-suites #f))
  ;; Only this admission section owns expander/module initialization. Public
  ;; expander exports and std/test objects are the normal gxtest suite protocol.
  ;; C FFI dependencies must be admitted before evaluating source suites;
  ;; the interpreter cannot evaluate their C declarations. This shared loader
  ;; cache survives every owner in the single runtime.
  (call-with-module-evaluation-trace
   (lambda ()
    ;; The SDK resolves author and macro-generated imports once. Evaluation
    ;; observes that selected context; no second source import graph is walked.
    (let* ((context (import-test-owner file))
         (exports (module-context-export context))
         (suites '()) (setup void) (cleanup void) (affinity #f) (exclusive? #f))
    (for-each
      (lambda (exported)
        (when (zero? (module-export-phi exported))
          (let* ((name (module-export-name exported))
                 (binding (core-resolve-module-export exported)))
            (cond
              ((eq? name 'test-worker-affinity)
               (set! affinity (eval (binding-id binding)))
               (unless (symbol? affinity) (error "invalid test Worker affinity" file affinity)))
              ((eq? name 'test-worker-exclusive?)
               (set! exclusive? (eval (binding-id binding)))
               (unless (boolean? exclusive?) (error "invalid test Worker exclusivity" file exclusive?)))
              ((eq? name 'test-setup!) (set! setup (eval (binding-id binding))))
              ((eq? name 'test-cleanup!) (set! cleanup (eval (binding-id binding))))
              ((source-test-suite-selected? name covered-names)
               (unless (and (module-binding? binding)
                            (eq? (module-context-id (module-binding-context binding))
                                 (module-context-id context)))
                 (error "test Suite must be owned by its declaring module" file name))
               (let (suite (eval (binding-id binding)))
                 (unless (TestSuite? suite) (error "invalid exported test suite" file name))
                 (when seen-suites
                   (when (table-ref seen-suites suite #f)
                     (error "duplicate test Suite object" file name (table-ref seen-suites suite)))
                   (table-set! seen-suites suite file))
                 (set! suites (cons suite suites)))))))) exports)
    (when (null? suites) (error "Scheme test module exports no suites" file))
    (make-test-owner file
      (TestHarness file (TestConfig verbosity: 6 capture-output?: #f)
                   (list (TestModule file (reverse suites) '() setup cleanup)))
      0 affinity exclusive?)))))

(def (call-with-owner-budget file emit thunk (budget 90))
  (let-values (((input output) (open-string-pipe '(buffering: #f))))
    (let* ((started (##current-time-point))
           ;; The caller is the persistent Worker. Only output supervision runs
           ;; elsewhere, preserving C ABI runtime thread ownership.
           (monitor
             (spawn
               (lambda ()
                 (let* ((last-progress started)
                        ;; One reader owns the pipe for the entire owner. It
                        ;; sends real messages to the shared console owner;
                        ;; no per-line threads or acknowledgement round trips.
                        (reader
                         (spawn
                           (lambda ()
                             (let read-output ((cases 0))
                               (let (line (read-line input))
                                 (set! last-progress (##current-time-point))
                                 (if (eof-object? line) cases
                                   (begin
                                     (emit "MODULE-OUTPUT " file " " line)
                                     (read-output (+ cases (if (string-prefix? "CASE-OK " line) 1 0)))))))))))
                   (let watch ()
                     (let* ((now (##current-time-point))
                            (remaining (min (- budget (- now started))
                                            (- 5 (- now last-progress))))
                            (result
                             (with-catch
                               (lambda (e)
                                 (emit "MODULE-BATCH-READER-FAILED " file)
                                 (display-exception e)
                                 (native-test-exit! 70))
                               (lambda () (thread-join! reader (max 0.001 remaining) 'quiet)))))
                       (if (eq? result 'quiet)
                         (let (now (##current-time-point))
                           (cond
                            ((>= (- now started) budget)
                             (emit "MODULE-BATCH-TIMEOUT " file " reason=total-budget")
                             (native-test-exit! 70))
                            ((>= (- now last-progress) 5)
                             (emit "MODULE-BATCH-TIMEOUT " file " reason=idle-timeout")
                             (native-test-exit! 70))
                            (else (watch))))
                         result))))))))
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

(def (main . arguments)
  ;; CI reaches this mode only after the compiled conformance gate succeeds.
  ;; Direct source qualification remains complete by default.
  (let* ((workers? (and (pair? arguments) (equal? (car arguments) "--workers")))
         (capacity
           (let (value (if workers?
                         (if (pair? (cdr arguments)) (string->number (cadr arguments)) #f)
                         (##core-count)))
             (unless (and (integer? value) (exact? value) (> value 0))
               (error "--workers requires a positive integer"))
             value))
         (arguments (if (and workers? (pair? (cdr arguments))) (cddr arguments) arguments))
         (remaining? (and (pair? arguments) (equal? (car arguments) "--remaining-conformance")))
         (roots (if remaining? (cdr arguments) arguments))
         (all-files (apply append (map test-files roots)))
         (covered-files (if remaining? (conformance-covered-test-files) '()))
         (covered (map (lambda (file) (path-normalize (path-expand file))) covered-files))
         (source-rows (if remaining? (conformance-source-suites) '()))
         (partial (map (lambda (row) (path-normalize (path-expand (string-append (car row) ".ss")))) source-rows))
         (files (filter (lambda (file)
                          (let (path (path-normalize (path-expand file)))
                            (or (not (member path covered)) (member path partial)))) all-files))
         (lock (make-mutex)) (pending '()) (failures '())
         (seen-suites (make-table test: eq?)) (logger #f))
    (when remaining?
      (unless (andmap (lambda (file) (member file (map (lambda (path) (path-normalize (path-expand path))) all-files))) covered)
        (error "remaining test roots omit conformance owners"))
      (displayln "TEST-PLAN compiled-owners=" (length covered) " source-owners=" (length files) " source-only-suites=" (length (apply append (map cdr source-rows))))
      (force-output))
    (unless (= (length all-files) (length (unique-test-files all-files)))
      (error "Scheme test suite has duplicate module owners"))
    (when (null? files) (error "Scheme test suite has no modules" roots))
    (def (covered-suite-names file)
      (let (path (path-normalize (path-expand file)))
        (if (and remaining? (member path partial))
          (let (owner (find (lambda (owner) (equal? path (path-normalize (path-expand owner)))) covered-files))
            (conformance-imported-test-names (path-strip-extension owner)))
          '())))
    (def (locked thunk)
      (mutex-lock! lock)
      (try (thunk) (finally (mutex-unlock! lock))))
    (def (emit . values)
      ;; Readers must ingest actual owner progress independently of global
      ;; console throughput. Fatal watchdog receipts flush synchronously before
      ;; native exit; ordinary messages preserve mailbox order.
      (if (and logger (not (member (car values)
                              '("MODULE-BATCH-TIMEOUT " "MODULE-BATCH-READER-FAILED "))))
        (thread-send logger values)
        (locked (lambda () (apply displayln values) (force-output)))))
    (def (flush-logger!)
      (when logger
        (let (barrier (make-barrier 1))
          (thread-send logger barrier)
          (barrier-wait! barrier))))
    (def (stop-logger!)
      (when logger
        (let (active logger)
          (set! logger #f)
          (thread-send active (void))
          (thread-join! active))))
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
    ;; One console owner flushes bounded batches of real messages. No timer
    ;; writes progress and no per-line acknowledgement blocks the pipe reader.
    (set! logger
      (spawn
       (lambda ()
         (let loop ((message (thread-receive)))
           (let (stop?
                 (locked
                  (lambda ()
                    (let (stop?
                          (let drain ((message message) (remaining 128))
                            (cond ((void? message) #t)
                                  ((not message) #f)
                                  ((Barrier? message)
                                   (force-output)
                                   (barrier-post! message)
                                   #f)
                                  (else
                                   (apply displayln message)
                                   (if (= remaining 1) #f
                                     (drain (thread-receive 0 #f) (- remaining 1)))))))
                      (force-output) stop?))))
             (unless stop? (loop (thread-receive))))))))
    (try
      ;; Dynamic module admission completes before any test Worker runs. The
      ;; runtime loader/expander is a shared preparation owner, not a worker job.
      (set! pending
        (map (lambda (file)
               (emit "MODULE-PREPARE-START " file)
               (let (started (##current-time-point))
                 (let-values (((owner _)
                                (call-with-owner-budget file emit
                                  (lambda () (prepare-test-owner file (covered-suite-names file) seen-suites)))))
                   (test-owner-preparation-seconds-set! owner (- (##current-time-point) started))
                   (emit "MODULE-PREPARE-OK " file " seconds=" (test-owner-preparation-seconds owner))
                   owner))) files))
      (emit "TEST-SUITE-WORKERS " (min capacity (length files)))
      ;; CPU/wall admission cannot share an execution phase with other owners.
      ;; This is declared metadata, not a filename-specific scheduling rule.
      (let (exclusive (filter (lambda (owner) (test-owner-exclusive? owner)) pending))
        (set! pending (filter (lambda (owner) (not (test-owner-exclusive? owner))) pending))
        ;; The caller is persistent Worker zero across both phases. Affine ABI
        ;; owners must not migrate from a spawned Worker to the caller later.
        (let (workers (map (lambda (index) (spawn (lambda () (worker index))))
                          (iota (max 0 (- (min capacity (length pending)) 1)) 1)))
          (worker 0)
          (for-each thread-join! workers))
        (when (and (null? failures) (pair? exclusive))
          ;; Official Barrier acknowledges a real console drain, once per
          ;; phase. It emits no heartbeat and adds no per-line round trip.
          (flush-logger!)
          (emit "TEST-SUITE-EXCLUSIVE modules=" (length exclusive))
          (set! pending exclusive)
          (worker 0)))
      (stop-logger!)
      (unless (null? failures) (raise (cdar failures)))
      (emit "TEST-SUITE-OK modules=" (length files))
      (emit "OK")
    (finally (stop-logger!)))
    ;; All workers joined and every owner passed. The test child must release
    ;; its process now; SDK finalizers are outside the harness contract.
    (native-test-exit! 0)))
