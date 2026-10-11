;;; Forged or incomplete observations must not qualify an optimization.
(import :std/test :std/io/tempfile
        (only-in :gerbil-parser/t/benchmarks/parser-stage-cost/receipt read-parser-component-receipt))
(export component-receipt-test)
(def (sample index (bytes 64))
  (list 'PARSER-COMPONENT-SAMPLE 'control
        (list (cons 'sample index) '(cpu-ms . 2.) '(gc-count . 0.)
              (cons 'allocated-bytes bytes) (cons 'allocation-counter-delta bytes))))
(def (summary (cpu 2.))
  (list 'PARSER-COMPONENT-SUMMARY
        (list '(stage . control) '(parsesPerSample . 4) '(sampleCount . 2)
              '(allocationScope . single-caller) '(allocationSampleCount . 2)
              '(allocatedBytesPerParse . 16.) (cons 'cpuP50Ms cpu))))
(def (admit rows (complete? #t))
  (let (path (make-temporary-file-name "parser-component-receipt-"))
    (try
      (begin
        (call-with-output-file path
          (lambda (port)
            (for-each (lambda (row) (write row port) (newline port)) rows)
            (when complete? (display "CONTROL-OK\n" port))
            (display "PROCESS-RESULT elapsed=1 exit=0 reason=#f missing=()\n" port)))
        (read-parser-component-receipt path "CONTROL-OK" 1 4 2))
      (finally (when (file-exists? path) (delete-file path))))))
(def (rejected? rows (complete? #t))
  (with-catch
    (lambda (e) (equal? (error-message e) "parser component receipt rejected"))
    (lambda () (admit rows complete?) #f)))
(def component-receipt-test
  (test-suite "component evidence admission"
    (test-case "complete independent samples retain exact allocation"
      (let (result (admit (list (sample 0) (sample 1) (summary))))
        (check (caddr result) => 2)
        (check (hash-get (cadr result) 'control) => '(64 64))))
    (test-case "repeated sample identities and summaries are rejected"
      (check (rejected? (list (sample 0) (sample 0) (summary))) => #t)
      (check (rejected? (list (sample 0) (sample 1) (summary) (summary))) => #t))
    (test-case "changed counters and stale CPU summaries are rejected"
      (check (rejected? (list (sample 0) (sample 1 72) (summary))) => #t)
      (check (rejected? (list (sample 0) (sample 1) (summary 3.))) => #t))
    (test-case "missing completion and missing observations are rejected"
      (check (rejected? (list (sample 0) (sample 1) (summary)) #f) => #t)
      (check (rejected? (list (sample 0) (summary))) => #t))))
