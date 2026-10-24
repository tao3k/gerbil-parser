;;; Controlled children qualify cancellation and isolation, never existing jobs.
(import :gerbil-parser/t/support/qualification/process :gerbil-parser/t/support/qualification/ownership :std/io/tempfile :std/misc/ports)
(def (check condition label . details)
  (unless condition (apply error "process control failed" label details)))
;;; Drain initialization output before applying pressure to the executed child.
;;; Otherwise library admission can fill the pipe before it creates ready.
(def (backpressure-output pipe ready)
 (let* ((started (##current-time-point)) (paused? #f)
        (capture (open-output-u8vector))
        (reader
          (spawn
            (lambda ()
              (let (buffer (make-u8vector 65536))
                (let loop ()
                  (let (count (read-subu8vector buffer 0 65536 pipe 1))
                    (unless (zero? count)
                      (write-subu8vector buffer 0 count capture)
                      (when (and (not paused?) (file-exists? ready))
                        (set! paused? #t)
                        ;; Deliberately exceed the unchanged 0.8s idle fence.
                        (thread-sleep! 1.2))
                      (loop)))))
              (get-output-u8vector capture)))))
  (try
    (let loop ()
      (unless (file-exists? ready)
        (when (> (- (##current-time-point) started) 4)
          (error "flood child did not start" (process-status pipe 0 #f)
                 (thread-join! reader 0 'pending)
                 (u8vector-length (get-output-u8vector capture))))
        (thread-sleep! 0.05) (loop)))
    ;; This is the target's one-time real acknowledgement, not a heartbeat.
    (displayln "PROCESS-CONTROL-READY unicode-backpressure") (force-output)
    (let (bytes (thread-join! reader (max 0.001 (- 6 (- (##current-time-point) started))) 'incomplete))
      (check (u8vector? bytes) 'backpressure-completion
             'elapsed (- (##current-time-point) started)
             'captured (u8vector-length (get-output-u8vector capture))
             'status (process-status pipe 0 'pending))
      (check paused? 'backpressure-exercised)
      bytes)
    (finally (thread-terminate! reader) (close-port capture)))))
(def (main fixture)
 (let (directory (make-temporary-file-name "parser-process-controls-"))
  (create-directory directory)
  (try
   (def (run mode (budget 1.5) (idle 0.8))
    (run-observed-process [fixture mode] (path-expand (string-append mode ".log") directory)
      timeout: budget idle-timeout: idle))
   ;; Exercise the first child-module load concurrently, before sequential
   ;; controls can warm its objects. Keep the ordinary identity deadlines.
   (let (workers
         (map (lambda (index)
                (spawn (lambda ()
                         (run-observed-process [fixture "identity"]
                           (path-expand (string-append "cold-" (number->string index) ".log") directory)
                           timeout: 3 idle-timeout: 1.5))))
              '(0 1 2 3 4 5)))
    (for-each
     (lambda (worker)
      (let (result (thread-join! worker))
       (check (and (= (process-result-status result) 0)
                   (not (process-result-reason result))
                   (receipt-present? "^NATIVE-EXEC-IDENTITY-OK$"
                                     (utf8->string (process-result-output result))))
              'concurrent-cold-identity (process-result-status result)
              (process-result-reason result) (process-result-phase result)
              (process-result-elapsed result))))
     workers))
   (displayln "PROCESS-CONTROL-OK concurrent-cold-identity count=6") (force-output)
   ;; The process contract must not consume an unrelated caller notification.
   (thread-send (current-thread) '(caller-notification))
   (let (result (run "identity" 3 1.5))
    (check (= (process-result-status result) 0) 'exec-session-leader
           (process-result-status result) (process-result-reason result)
           (process-result-elapsed result) (process-result-phase result)
           (utf8->string (process-result-output result)))
    (check (not (process-result-reason result)) 'exec-identity-reason)
    (check (receipt-present? "^NATIVE-EXEC-IDENTITY-OK$"
                            (utf8->string (process-result-output result))) 'exec-identity))
   (check (equal? (thread-receive 0 #f) '(caller-notification)) 'caller-mailbox-preserved)
   (let (result (run-observed-process [fixture "arguments" "λ a;$(not-a-command)"]
                   (path-expand "arguments.log" directory) timeout: 3 idle-timeout: 1.5))
    (check (= (process-result-status result) 0) 'exec-argv)
    (check (not (process-result-reason result)) 'exec-argv-reason))
   (let (result (run-observed-process ["/nonexistent/gerbil-parser-controlled-missing"]
                   (path-expand "missing.log" directory) timeout: 3 idle-timeout: 1.5))
    (check (not (= (process-result-status result) 0)) 'exec-failure-status)
    (check (eq? (process-result-reason result) 'error-output) 'exec-failure-reason))
   (displayln "PROCESS-CONTROL-OK exec-identity utf8-argv exec-failure") (force-output)
   (for-each
    (lambda (row)
     (let (result (run (car row)))
      (check (eq? (process-result-reason result) (cadr row)) (car row)
             'expected (cadr row) 'actual (process-result-reason result)
             'status (process-result-status result) 'elapsed (process-result-elapsed result)
             'phase (process-result-phase result))
      (displayln "PROCESS-CONTROL-OK " (car row)) (force-output)))
    '(("silent" idle-timeout) ("continuous" total-timeout)
      ("closed-output" idle-timeout) ("error" error-output)))
   (let* ((escaped (path-expand "escaped-marker" directory))
          (unrelated (path-expand "unrelated-marker" directory))
          (other (open-process [path: fixture arguments: ["unrelated" unrelated]])))
    (try
     (let (result (run-observed-process [fixture "escaped" escaped]
                       (path-expand "escaped.log" directory) timeout: 1.5 idle-timeout: 0.8))
      (check (eq? (process-result-reason result) 'idle-timeout) 'escaped-timeout)
      (check (receipt-present? "^ESCAPED-SESSION " (utf8->string (process-result-output result))) 'escaped-started))
     (check (= (process-status other 4 -1) 0) 'unrelated-exit)
     (thread-sleep! 1)
     (check (file-exists? unrelated) 'unrelated-survived)
     (check (not (file-exists? escaped)) 'escaped-stopped)
     (finally (close-port other)))
    (displayln "PROCESS-CONTROL-OK escaped-session-stopped unrelated-survived") (force-output))
   (for-each
    (lambda (padded?)
     (let* ((label (if padded? "padded-flood" "flood"))
            (ready (path-expand (string-append label "-ready") directory))
            (log (path-expand (string-append label ".log") directory))
            ;; More than a Darwin pipe's capacity, before qualification starts.
            ;; This is fixture data, not progress manufactured by the watchdog.
            (prelude (if padded?
                       ["-e" "(write-string (make-string 65536 #\\p)) (newline) (force-output)"] '()))
            (pipe (open-process [path: "gxi" arguments:
                    (append prelude
                     ["qualify.ss" "run" "--timeout" "6" "--idle-timeout" "0.8"
                      "--log" log "--require" "^OK$" "--" fixture "flood" ready])
                    stderr-redirection: #t])))
      (close-output-port pipe)
      (try
       (let* ((bytes (backpressure-output pipe ready)) (text (utf8->string bytes))
              (saved (call-with-input-file log read-all-as-u8vector)))
        (check (= (process-status pipe 2 -1) 0) 'backpressure-exit)
        (check (= (length (filter (lambda (line) (string-prefix? "PIPE-PROGRESS " line))
                                 (string-split text #\newline))) 2000) 'console-byte-completeness)
        (check (= (u8vector-length saved) (+ (* 2000 (+ 15 2000)) 3)) 'logged-byte-completeness)
        (when padded?
          (check (string-prefix? (make-string 65536 #\p) text) 'startup-byte-completeness))
        (check (receipt-present? "^OK$" text) 'flood-final-receipt))
       (finally (close-port pipe)))
      (displayln "PROCESS-CONTROL-OK unicode-backpressure " label) (force-output)))
    '(#f #t))
   (let (result (run "early-error" 3 1.5))
    (check (eq? (process-result-reason result) 'error-output) 'early-error)
    (check (< (process-result-elapsed result) 2.5) 'early-error-stopped)
    (displayln "PROCESS-CONTROL-OK immediate-error-cancellation") (force-output))
   (let (result (run "signal" 3 1.5))
    (check (eq? (process-result-reason result) 'signal-exit) 'signal-distinguished)
    (check (= (process-result-status result) 143) 'signal-status))
   (check (= (process-result-status (run "failure" 3 1.5)) 42) 'nonzero-propagated)
   (check (not (receipt-present? "^FINAL-OK$" "CASE-OK\nHARNESS-OK\n")) 'missing-final-receipt)
   (let (plan (ownership-plan 100 '((100 10 100) (101 100 100) (102 101 102)
                                  (103 102 102) (200 10 200) (201 200 200)) 100))
    (check (and (member 100 (cadr plan)) (member 102 (cadr plan)) (andmap (lambda (group) (member group '(100 102))) (cadr plan))) 'owned-groups-only))
   (displayln "PROCESS-OWNERSHIP-CONTROLS-OK") (force-output)
   (finally (delete-file-or-directory directory #t))))
 (exit 0))
