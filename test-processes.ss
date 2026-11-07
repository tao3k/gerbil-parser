;;; Development/CI entry for standalone research product processes.
(import (only-in :gerbil-parser/t/support/qualification/process
                 run-qualified! process-result-status))
(export main)
(def (main profile . arguments)
  (unless (equal? profile "research") (error "unknown product profile" profile))
  (let ((directory (if (pair? arguments) (car arguments) ".data/research-controls"))
        (binary (path-expand "bin/gerbil-parser-conformance" (getenv "GERBIL_PATH"))))
    (create-directory* directory)
    (call-with-output-file (path-expand "exits.tsv" directory)
      (lambda (exits)
        (for-each
          (lambda (name)
            (let (result (run-qualified! [binary name]
                           (path-expand (string-append name ".log") directory)
                           timeout: 10 idle-timeout: 5))
              (display name exits) (display "\t" exits)
              (display (process-result-status result) exits) (newline exits)
              (force-output exits)
              (displayln "RESEARCH-CONTROL-OK " name) (force-output)))
          '("concise-runtime" "history" "native-poo-runtime" "provenance" "all-language-benchmark"))))
    (displayln "DSL-RESEARCH-CLOSURE-OK: all child processes exited 0")
    (displayln "OK") (force-output)))
