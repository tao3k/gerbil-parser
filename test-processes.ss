;;; Native qualification of process-owning test entries.
(displayln "PROCESS-QUALIFICATION-START") (force-output)
(load "t/fixtures/tla-sany-differential/preload.ss")
(prefer-compiled-interfaces!)
(call-with-compiled-interface-trace
 (lambda () (eval '(import :std/misc/process :std/os/signal))))

(import :std/misc/process :std/os/signal)

(defstruct process-case (name arguments exit marker budget idle) transparent: #t)

(def (qualification-cases profile)
  (cond
   ((equal? profile "conformance")
    (map (lambda (row)
           (process-case (car row)
             ["gxi" "scripts/tests/conformance-runner-test.ss" (car row)]
             (cadr row) (caddr row) 10 5))
         '(("success" 0 "CONFORMANCE-CONTROL-OK success")
           ("duplicate" 70 "duplicate conformance Suite")
           ("failure" 1 "negative control")
           ("timeout" 70 "CONFORMANCE-GROUP-TIMEOUT timeout"))))
   ((equal? profile "workers")
    (setenv "GERBIL_TEST_CORES" "2")
    (map (lambda (row)
           (process-case (car row)
             (append ["gxi" "t/fixtures/tla-sany-differential/test-suite.ss"]
               (map (lambda (name)
                      (string-append "t/fixtures/tla-sany-differential/" name ".ss"))
                    (cadr row)))
             (caddr row) (cadddr row) 30 #f))
         '(("shared-runtime" ("worker-left" "worker-right") 0 #f)
           ("runtime-affinity" ("affinity-left" "affinity-right") 0 #f)
           ("failure-propagation" ("worker-failure") nonzero #f)
           ("silence-watchdog" ("worker-timeout") 70 #f)
           ("duplicate-ownership" ("worker-left" "worker-left") nonzero #f)
           ("reexport-ownership" ("worker-reexport") nonzero #f)
           ("suite-object-ownership" ("worker-suite-alias") 70 "duplicate test Suite object"))))
   ((equal? profile "research")
    (let (binary (path-expand "bin/gerbil-parser-conformance" (getenv "GERBIL_PATH")))
      (cons (process-case "semantic" [binary "semantic"] 0
                           "CONFORMANCE-SEMANTIC-OK groups=" 240 5)
        (map (lambda (name) (process-case name [binary name] 0 #f 10 5))
             '("concise-runtime" "history" "native-poo-runtime" "provenance"
               "all-language-benchmark")))))
   (else (error "unknown process qualification profile" profile))))

(def (qualify-process! case directory)
  (let* ((name (process-case-name case))
         (started (##current-time-point))
         (captured (open-output-u8vector))
         (log (open-output-file (path-expand (string-append name ".log") directory)))
         (raw-status #f)
         (timer #f)
         (expired? #f))
    (displayln "PROCESS-START " name) (force-output)
    (try
     (run-process (process-case-arguments case) stderr-redirection: #t
       check-status:
       (lambda (status _)
         ;; Stop the timer as soon as the child has been reaped; validation must
         ;; never signal a PID whose ownership has already ended.
         (when timer (thread-terminate! timer) (set! timer #f))
         (set! raw-status status))
       coprocess:
       (lambda (process)
         (close-output-port process)
         ;; Cover the exit wait as well as output reads, including a child that
         ;; closes its output before it terminates. These entries own Scheme
         ;; threads, not external compiler trees; build supervision stays outside.
         (set! timer
           (spawn (lambda ()
                    (thread-sleep! (process-case-budget case))
                    (set! expired? #t)
                    (with-catch void (lambda () (kill (process-pid process) SIGKILL))))))
         (let ((buffer (make-u8vector 8192)))
           (let loop ()
             (let* ((remaining (- (process-case-budget case)
                                  (- (##current-time-point) started)))
                    (wait (max 0 (if (process-case-idle case)
                                    (min remaining (process-case-idle case)) remaining)))
                    (reader (spawn (lambda ()
                                     (read-subu8vector buffer 0 8192 process 1))))
                    (count (thread-join! reader wait 'timeout)))
               (when (eq? count 'timeout)
                 (kill (process-pid process) SIGKILL)
                 (thread-terminate! reader)
                 (error "process qualification deadline exceeded" name))
               (unless (zero? count)
                 (write-subu8vector buffer 0 count captured)
                 (write-subu8vector buffer 0 count log) (force-output log)
                 (write-subu8vector buffer 0 count (current-output-port)) (force-output)
                 (loop)))))))
     ;; Gambit preserves signal termination in the low byte. A signal cannot
     ;; satisfy an expected normal failure exit.
     (let ((status (quotient raw-status 256)) (expected (process-case-exit case)))
       (unless (and (not expired?) (zero? (modulo raw-status 256))
                    (if (eq? expected 'nonzero) (> status 0) (= status expected)))
         (error "unexpected process exit" name raw-status))
       (let (marker (process-case-marker case))
         (when (and marker
                    (not (string-contains (utf8->string (get-output-u8vector captured)) marker)))
           (error "missing process receipt" name marker)))
       status)
     (finally
      (when timer (thread-terminate! timer))
      (close-port log) (close-port captured)))))

(def (main profile . arguments)
  (let* ((cases (qualification-cases profile))
         (directory (if (pair? arguments) (car arguments)
                        (string-append ".data/" profile "-controls")))
         (prefix (cond ((equal? profile "conformance") "CONFORMANCE-CONTROL-OK ")
                       ((equal? profile "workers") "WORKER-CONTROL-OK ")
                       (else "RESEARCH-CONTROL-OK "))))
    (create-directory* directory)
    (call-with-output-file (path-expand "exits.tsv" directory)
      (lambda (exits)
        (for-each (lambda (case)
                    (let (status (qualify-process! case directory))
                      (display (process-case-name case) exits) (display "\t" exits)
                      (display status exits) (newline exits)
                      (force-output exits)
                      (displayln prefix (process-case-name case)) (force-output))) cases)))
    (when (equal? profile "research")
      (displayln "DSL-RESEARCH-CLOSURE-OK: all child processes exited 0"))
    (displayln "OK") (force-output)
    (exit 0)))
