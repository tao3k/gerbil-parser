;;; Qualification must finish large logs and preserve streaming error framing.
(import :std/test :std/io/tempfile
        :gerbil-parser/t/support/qualification/process)
(export qualification-output-test)
(def (with-output-result mode proc)
  (let ((log (make-temporary-file-name "parser-output-test-"))
        (console (open-output-u8vector)))
    (try
      ;; The process protocol tees raw bytes; the Suite captures text output.
      ;; Supply a byte sink only while the real child executes.
      (let (result (parameterize ((current-output-port console))
                    (run-observed-process ["gxi" "t/fixtures/qualification/output.ss" mode]
                                          log timeout: 30 idle-timeout: 5)))
        (when (equal? mode "large")
          (write (list 'QUALIFICATION-OUTPUT-RESULT
                       'bytes (u8vector-length (process-result-output result))
                       'elapsed (process-result-elapsed result)
                       'status (process-result-status result)
                       'reason (process-result-reason result)))
          (newline) (force-output))
        (proc result))
      (finally
        (close-output-port console)
        (when (file-exists? log) (delete-file log))))))
(def qualification-output-test
  (test-suite "qualification output framing"
    (test-case "ordinary identifier prefixes do not become errors"
      (with-output-result "ordinary"
        (lambda (result)
          (check (process-result-status result) => 0)
          (check (process-result-reason result) => #f))))
    (test-case "line, punctuation and Gambit errors fail closed"
      (for-each
       (lambda (mode)
         (with-output-result mode
           (lambda (result) (check (process-result-reason result) => 'error-output))))
       '("error" "punctuation" "gambit")))
    (test-case "bare error at EOF survives a buffer boundary"
      (with-output-result "boundary"
        (lambda (result) (check (process-result-reason result) => 'error-output))))
    (test-case "completed eight MiB log has bounded finalization"
      (let (started (##current-time-point))
        (with-output-result "large"
          (lambda (result)
            (check (process-result-status result) => 0)
            (check (process-result-reason result) => #f)
            (check (u8vector-length (process-result-output result)) => (* 8 1024 1024))
            ;; Include result construction after the child monitor stops.
            (check (< (- (##current-time-point) started) 30) => #t)))))))
