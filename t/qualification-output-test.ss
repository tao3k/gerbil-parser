(import :gerbil-parser/language-test-support)
;;; Qualification must finish large logs and preserve streaming error framing.
(import :std/test :std/io/tempfile
        (only-in :std/text/pregexp pregexp pregexp-match)
        :gerbil-parser/t/support/qualification/process)
(import (only-in :gerbil-parser/t/support/qualification/compiler-output compiler-output-observer))

(def (with-compiler-outputs proc)
  (let (directory (make-temporary-file-name "parser-compiler-output-"))
    (create-directory directory)
    (try (proc directory) (finally (delete-file-or-directory directory #t)))))

(def (write-compiler-output directory name text)
  (call-with-output-file (path-expand name directory) (lambda (port) (display text port))))

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
(deflanguage-parser-tests qualification-output-test "qualification output framing"
  (engine)
    (scenario "cached and relocated compiler products cannot supply progress"
      (using (with-compiler-outputs) (directory)
        (act (write-compiler-output directory "cached.rmeta" "cached"))
        (scope ((observe (compiler-output-observer directory 0)))
          (equal (observe) #f)
          (act (rename-file (path-expand "cached.rmeta" directory) (path-expand "moved.rmeta" directory)))
          (equal (observe) #f)
          (equal (observe) #f))))
    (scenario "only fresh nonempty compiler products advance the observer"
      (using (with-compiler-outputs) (directory)
        (scope ((observe (compiler-output-observer directory 0)))
          (act (thread-sleep! 0.02))
          (act (write-compiler-output directory "heartbeat.log" "still alive"))
          (act (write-compiler-output directory "empty.rmeta" ""))
          (equal (observe) #f)
          (act (write-compiler-output directory "module.rmeta" "metadata"))
          (equal (observe) #t)
          (equal (observe) #f)
          (act (write-compiler-output directory "module.rmeta" "more metadata"))
          (equal (observe) #t)
          (act (delete-file (path-expand "module.rmeta" directory)))
          (equal (observe) #f))))
    (scenario "batched receipts preserve regexp line semantics and missing order"
      (sequential ((text "first\nx42\n\nliteral.end\nlast")
             (patterns '("^first$" "^x[0-9]+$" "literal\\.end" "^last$" "^absent$" "first\nx42" "^absent$"))
             (reference (filter (lambda (pattern)
                                  (not (any (lambda (line) (pregexp-match pattern line))
                                            (string-split text #\newline)))) patterns)))
        (equal (missing-process-receipts patterns text)  reference)
        (equal reference  '("^absent$" "first\nx42" "^absent$"))
        (equal (not (not (receipt-present? (pregexp "^x[0-9]+$") text)))  #t)
        (equal (missing-process-receipts (list (pregexp "^first$")) text)  '())
        (equal (missing-process-receipts '("^$" "^absent$") "")  '("^$" "^absent$"))
        (raises (missing-process-receipts '("[") text) true)
        (raises (missing-process-receipts '("(?i)abc") text) true)
        ;; An empty log has no lines: the original matcher never compiles.
        (equal (missing-process-receipts '("(?i)abc") "")  '("(?i)abc")))
      ;; Compare against the original line-by-line matcher for constructs that
      ;; must bypass the prefix guard as well as literal/anchored receipts.
      (scope ((patterns '("A|B" "A?BC" "[AB]CD" "(?i:abc)" "(AB)CD" "A.*B"
                        "^ABSENT$" "literal\\.end" "a+" "^$" "\\bword\\b" "雪."))
            (samples '("" "A" "B" "BC" "BCD" "abc" "ABC" "ABCD" "a\nB" "literal.end\n雪人" "word")))
        (each (text) (samples)
           (scope ((reference (filter (lambda (pattern)
                                     (not (any (lambda (line) (pregexp-match pattern line))
                                               (string-split text #\newline)))) patterns)))
             (equal (missing-process-receipts patterns text)  reference)))))
    (scenario "ordinary identifier prefixes do not become errors"
      (using (with-output-result "ordinary") (result)
          (equal (process-result-status result)  0)
          (equal (process-result-reason result)  #f)))
    (scenario "line, punctuation and Gambit errors fail closed"
      (each (mode) ('("error" "punctuation" "gambit"))
         (using (with-output-result mode) (result) (equal (process-result-reason result)  'error-output))))
    (scenario "bare error at EOF survives a buffer boundary"
      (using (with-output-result "boundary") (result) (equal (process-result-reason result)  'error-output)))
    (scenario "completed eight MiB log has bounded finalization"
      (scope ((started (##current-time-point)))
        (using (with-output-result "large") (result)
            (equal (process-result-status result)  0)
            (equal (process-result-reason result)  #f)
            (equal (u8vector-length (process-result-output result))  (* 8 1024 1024))
            (equal (missing-process-receipts '("^NOTE x+$" "^ABSENT$" "^NOTE" "^ABSENT$")
                     (utf8->string (process-result-output result)))  '("^ABSENT$" "^ABSENT$"))
            ;; Include result construction and receipt admission after the monitor stops.
            (equal (< (- (##current-time-point) started) 30)  #t)))))
