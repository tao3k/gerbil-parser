;;; Qualification must finish large logs and preserve streaming error framing.
(import :std/test :std/io/tempfile
        (only-in :std/text/pregexp pregexp pregexp-match)
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
    (test-case "batched receipts preserve regexp line semantics and missing order"
      (let* ((text "first\nx42\n\nliteral.end\nlast")
             (patterns '("^first$" "^x[0-9]+$" "literal\\.end" "^last$" "^absent$" "first\nx42" "^absent$"))
             (reference (filter (lambda (pattern)
                                  (not (any (lambda (line) (pregexp-match pattern line))
                                            (string-split text #\newline)))) patterns)))
        (check (missing-process-receipts patterns text) => reference)
        (check reference => '("^absent$" "first\nx42" "^absent$"))
        (check (not (not (receipt-present? (pregexp "^x[0-9]+$") text))) => #t)
        (check (missing-process-receipts (list (pregexp "^first$")) text) => '())
        (check (missing-process-receipts '("^$" "^absent$") "") => '("^$" "^absent$"))
        (check-exception (missing-process-receipts '("[") text) true)
        (check-exception (missing-process-receipts '("(?i)abc") text) true)
        ;; An empty log has no lines: the original matcher never compiles.
        (check (missing-process-receipts '("(?i)abc") "") => '("(?i)abc")))
      ;; Compare against the original line-by-line matcher for constructs that
      ;; must bypass the prefix guard as well as literal/anchored receipts.
      (let ((patterns '("A|B" "A?BC" "[AB]CD" "(?i:abc)" "(AB)CD" "A.*B"
                        "^ABSENT$" "literal\\.end" "a+" "^$" "\\bword\\b" "雪."))
            (samples '("" "A" "B" "BC" "BCD" "abc" "ABC" "ABCD" "a\nB" "literal.end\n雪人" "word")))
        (for-each
         (lambda (text)
           (let (reference (filter (lambda (pattern)
                                     (not (any (lambda (line) (pregexp-match pattern line))
                                               (string-split text #\newline)))) patterns))
             (check (missing-process-receipts patterns text) => reference))) samples)))
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
            (check (missing-process-receipts '("^NOTE x+$" "^ABSENT$" "^NOTE" "^ABSENT$")
                     (utf8->string (process-result-output result))) => '("^ABSENT$" "^ABSENT$"))
            ;; Include result construction and receipt admission after the monitor stops.
            (check (< (- (##current-time-point) started) 30) => #t)))))))
