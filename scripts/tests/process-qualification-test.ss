;;; Exercise production process contracts with real native children.
(load "test-processes.ss")
(def make-case
  (eval '(lambda (name command expected marker budget idle)
            (process-case name command expected marker budget idle))))
(def root (path-expand ".data/process-qualification-controls"))
(create-directory* root)
(def (probe name code expected marker budget idle)
  (qualify-process!
   (make-case name ["gxi" "-e" code] expected marker budget idle) root))
(def (reject! name thunk)
  (unless (with-catch (lambda (_) #t) (lambda () (thunk) #f))
    (error "negative process control was accepted" name))
  (displayln "PROCESS-CONTROL-OK " name) (force-output))
(unless (= 17 (probe "exit-status" "(exit 17)" 17 #f 2 1))
  (error "normal exit status changed"))
(displayln "PROCESS-CONTROL-OK exit-status")
(probe "fragments" "(display \"fragment\") (force-output) (display \"diagnostic\" (current-error-port)) (force-output (current-error-port)) (exit 0)"
       0 "fragmentdiagnostic" 2 1)
(displayln "PROCESS-CONTROL-OK partial-output")
(reject! "wrong-exit" (lambda () (probe "wrong-exit" "(exit 17)" 0 #f 2 1)))
(reject! "missing-marker" (lambda () (probe "missing-marker" "(exit 0)" 0 "MISSING" 2 1)))
(reject! "idle-timeout" (lambda () (probe "idle" "(thread-sleep! 2) (exit 0)" 0 #f 2 0.1)))
(reject! "total-timeout"
 (lambda () (probe "total" "(let loop () (display \".\") (force-output) (thread-sleep! 0.02) (loop))"
                    0 #f 0.15 0.1)))
;; EOF is not process completion: the total fence also covers process-status.
(reject! "closed-output-timeout"
 (lambda () (probe "closed-output" "(close-output-port (current-output-port)) (close-output-port (current-error-port)) (thread-sleep! 2) (exit 0)"
                    0 #f 0.15 #f)))
(displayln "PROCESS-QUALIFICATION-CONTROLS-OK") (force-output)
(exit 0)
