;;; Dense resume from the initial frontier is the independent existing control.
(import :std/test
        (only-in :gerbil-parser/t/fixtures/runtime-record-assignments/languages/records/grammar records-parser)
        (only-in :gerbil-parser/src/compiler/machine parser-machine-runtime)
        (only-in :gerbil-parser/src/runtime/parser parse-source/checkpoints parse-source/checkpoints/resume
                 current-source-stream-observer)
        (only-in :gerbil-parser/src/runtime/lr-parser lr-initial-checkpoint current-lr-recognition-observer))
(export sparse-capture-test)
(def (capture-observation source spacing dense?)
  (let ((root #f) (scans '()))
    (parameterize ((current-lr-recognition-observer (lambda (value) (set! root value)))
                   (current-source-stream-observer
                    (lambda (token mode fragment?) (set! scans (cons (list token mode fragment?) scans)))))
      (let (result
            (call-with-values
             (lambda ()
               (if dense?
                 (parse-source/checkpoints/resume records-parser source spacing
                   (lr-initial-checkpoint (parser-machine-runtime records-parser) '()) '() '() 0 0 #f)
                 (parse-source/checkpoints records-parser source spacing))) list))
        ;; Includes every checkpoint field and the complete grammar tree, not
        ;; merely the public artifact or a snapshot count.
        (list result root (reverse scans))))))
(def sparse-capture-test
  (test-suite "sparse initial LR capture equals dense initial resume"
    (test-case "all checkpoint fields match at dense and sparse spacings"
      (let (source (apply string-append (make-list 64 "a=1\n")))
        (for-each (lambda (spacing)
                    (check (capture-observation source spacing #f)
                           => (capture-observation source spacing #t))) '(1 2 2.0 32 1024))))
    (test-case "empty and trivia-only sources retain exact initial boundaries"
      (for-each (lambda (source)
                  (check (capture-observation source 2 #f)
                         => (capture-observation source 2 #t))) '("" " \t\n" "\r\n")))
    (test-case "Unicode strings and trivia retain modes and scan callback order"
      (for-each (lambda (source)
                  (check (capture-observation source 2 #f)
                         => (capture-observation source 2 #t)))
                '("a = \"猫\"\n b=2\n" "a=1\r\nb=2\r\n" "a=1\n \t")))
    (test-case "rejection and incomplete EOF retain the dense diagnostics"
      (for-each (lambda (source)
                  (check (capture-observation source 2 #f)
                         => (capture-observation source 2 #t)))
                '("a=" "a=1\n?" "a=\"unterminated" "a=1\n猫=2\n")))))
