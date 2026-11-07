;;; Diagnose startup phases only; this is not a process-deadline admission.
(import :gerbil-parser/t/support/qualification/process
        :std/io/tempfile :std/misc/ports
        (only-in :std/os/signal kill SIGKILL))
(export main)
(def (main fixture)
  (let (directory (make-temporary-file-name "parser-startup-controls-"))
    (create-directory directory)
    (try
      (def (probe label program arguments)
        (let* ((started (##current-time-point))
               (child (open-process [path: program arguments: arguments stderr-redirection: #t]))
               (status #f))
          (close-output-port child)
          (try
            (set! status (process-status child 3 #f))
            (unless status (error "startup diagnostic exceeded three seconds" label))
            (displayln "STARTUP-RESULT " label " elapsed="
                       (- (##current-time-point) started) " raw-status=" status)
            (display (utf8->string (read-all-as-u8vector child))) (force-output)
            (unless (zero? status) (error "startup diagnostic failed" label status))
            (finally
              (unless status
                (kill (process-pid child) SIGKILL)
                (process-status child 1 #f))
              (close-port child)))))
      (def phase-gxi "(displayln \"STARTUP-PHASE gxi \" (##current-time-point)) (force-output)")
      (probe "gxi-only" "gxi" ["-e" phase-gxi "-e" "(exit 0)"])
      (probe "native-child" qualification-process-child
             (process-child-arguments (path-expand "session" directory)
                                      (getenv "GERBIL_LOADPATH" "") [fixture "identity"]))
      (finally (delete-file-or-directory directory #t)))))
