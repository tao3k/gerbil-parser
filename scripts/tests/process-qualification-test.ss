;;; Real process fixtures; native Gerbil test owns Suite execution.
(import :std/test
        (only-in :gerbil-parser/t/support/qualification/process
                 run-observed-process run-qualified! process-result-reason
                 process-result-status process-result-phase))
(export process-qualification-test test-setup!)
(def root (path-expand ".data/process-qualification-controls"))
(def (test-setup!) (create-directory* root))
(def (observe name code budget idle)
  (run-observed-process ["gxi" "-e" code] (path-expand name root)
    timeout: budget idle-timeout: idle))
;; These directed status/receipt Cases retain their two-second total budget.
;; Silence is tested independently below; startup must not satisfy that subject.
(def (admit name code marker)
  (run-qualified! ["gxi" "-e" code] (path-expand name root)
    timeout: 2 idle-timeout: #f require: (if marker [marker] '())))
(def (timeout-probe! name code expected budget idle)
  (let (result (observe name code budget idle))
    (check (process-result-reason result) => expected)
    (check (not (zero? (process-result-status result))) => #t)
    ;; A bootstrap refusal is not evidence that the child reached EOF.
    (displayln "PROCESS-CONTROL-PHASE " name " reason=" expected
               " phase=" (process-result-phase result)) (force-output)))
(def process-qualification-test
  (test-suite "native process qualification admission"
    (test-case "normal exit retains its actual status"
      (let (result (observe "exit-status" "(exit 17)" 2 #f))
        (check (process-result-status result) => 17)
        (check (process-result-reason result) => #f)))
    (test-case "fragmented stdout and stderr retain one receipt"
      (admit "fragments"
        (object->string '(begin (display "fragment") (force-output) (display "diagnostic" (current-error-port)) (force-output (current-error-port)) (exit 0)))
        "fragmentdiagnostic"))
    (test-case "unexpected nonzero exit is rejected"
      (check-exception (admit "wrong-exit" "(exit 17)" #f) exception?))
    (test-case "missing output receipt is rejected"
      (check-exception (admit "missing-marker" "(exit 0)" "MISSING") exception?))
    (test-case "idle silence retains its original timeout classification"
      (timeout-probe! "idle-timeout" "(thread-sleep! 2) (exit 0)" 'idle-timeout 2 0.1))
    (test-case "active output cannot defeat the total deadline"
      (timeout-probe! "total-timeout"
        (object->string '(let loop () (display ".") (force-output) (thread-sleep! 0.02) (loop)))
        'total-timeout 0.15 #f))
    (test-case "total budget covers bootstrap and status after EOF"
      (timeout-probe! "closed-output-timeout"
        "(close-output-port (current-output-port)) (close-output-port (current-error-port)) (thread-sleep! 2) (exit 0)"
        'total-timeout 0.15 #f))))
