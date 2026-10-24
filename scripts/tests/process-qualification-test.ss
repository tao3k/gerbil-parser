;;; Exercise production process contracts with real native children.
(load "test-processes.ss")
(import (only-in :gerbil-parser/t/support/qualification/process
                 run-observed-process process-result-reason process-result-status process-result-phase))
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
(def (timeout-probe! name code expected budget idle)
  (let (result (run-observed-process ["gxi" "-e" code] (path-expand name root)
                                    timeout: budget idle-timeout: idle))
    (unless (and (eq? (process-result-reason result) expected)
                 (not (zero? (process-result-status result))))
      (error "wrong process timeout classification" name expected result))
    (displayln "PROCESS-CONTROL-OK " name " reason=" expected
               " phase=" (process-result-phase result)) (force-output)))
(timeout-probe! "idle-timeout" "(thread-sleep! 2) (exit 0)" 'idle-timeout 2 0.1)
;; Keep the original total budget; remove the competing idle fence from this
;; directed total-timeout case so bootstrap latency cannot satisfy the wrong test.
(timeout-probe! "total-timeout"
 "(let loop () (display \".\") (force-output) (thread-sleep! 0.02) (loop))" 'total-timeout 0.15 #f)
;; Total time includes bootstrap and process-status after EOF. Record the actual
;; phase; a bootstrap rejection is not evidence that the child reached its close.
(timeout-probe! "closed-output-timeout"
 "(close-output-port (current-output-port)) (close-output-port (current-error-port)) (thread-sleep! 2) (exit 0)"
 'total-timeout 0.15 #f)
(displayln "PROCESS-QUALIFICATION-CONTROLS-OK") (force-output)
(exit 0)
