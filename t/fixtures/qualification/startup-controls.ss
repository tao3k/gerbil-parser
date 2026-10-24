;;; Diagnose startup phases only; this is not a process-deadline admission.
(import :gerbil-parser/t/support/qualification/process
        :std/io/tempfile :std/misc/ports
        (only-in :std/os/signal kill SIGKILL))
(export main)
(def (main fixture)
  (let (directory (make-temporary-file-name "parser-startup-controls-"))
    (create-directory directory)
    (try
      (def environment
        (map (lambda (entry)
               (string-append (car entry) "="
                 (if (equal? (car entry) "GERBIL_LOADPATH")
                   (string-append qualification-library-root ":" (cdr entry))
                   (cdr entry))))
             (if (getenv "GERBIL_LOADPATH" #f) (get-environment-variables)
               (cons (cons "GERBIL_LOADPATH" "") (get-environment-variables)))))
      (def (probe label arguments)
        (let* ((started (##current-time-point))
               (child (open-process [path: "gxi" arguments: arguments
                                     environment: environment stderr-redirection: #t]))
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
      (def phase-module "(displayln \"STARTUP-PHASE module \" (##current-time-point)) (force-output)")
      (probe "gxi-only" ["-e" phase-gxi "-e" "(exit 0)"])
      (let (args (process-child-arguments (path-expand "session" directory)
                                         (getenv "GERBIL_LOADPATH" "") [fixture "identity"]))
        (probe "native-child"
               (append ["-e" phase-gxi] [(car args) (cadr args)]
                       ["-e" phase-module] (cddr args))))
      (finally (delete-file-or-directory directory #t)))))
