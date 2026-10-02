#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Fail on five seconds without real child output; retain a bounded batch.
(import (only-in :std/misc/process run-process)
        (only-in :std/os/signal kill SIGKILL))

(def (main program . arguments)
  (let (started (##current-time-point))
    (run-process (cons program arguments) stderr-redirection: #t
     coprocess:
     (lambda (process)
       ;; Checks consume arguments/files, never interactive input. Signal EOF
       ;; so gxi entries without main cannot wait in the REPL after finishing.
       (close-output-port process)
       (let loop ()
         (let* ((reader (spawn (lambda () (read-line process))))
                (line (thread-join! reader 5 'quiet)))
           (cond
            ((or (eq? line 'quiet) (> (- (##current-time-point) started) 600))
             (kill (process-pid process) SIGKILL)
             (thread-terminate! reader)
             (error "progress watchdog failed: five-second silence or batch timeout"))
            ((eof-object? line) (void))
            (else (displayln line) (force-output) (loop)))))))))
