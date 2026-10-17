(import :gerbil-parser/language-test-support)
;;; Dense resume from the initial frontier is the independent existing control.
(import :std/test
        (only-in :gerbil-parser/t/fixtures/runtime-record-assignments/languages/records/grammar records-parser)
        (only-in :gerbil-parser/src/compiler/machine parser-machine-runtime)
        (only-in :gerbil-parser/src/runtime/parser parse-source/checkpoints parse-source/checkpoints/resume
                 current-source-stream-observer)
        (only-in :gerbil-parser/src/runtime/lr-parser lr-initial-checkpoint current-lr-recognition-observer))

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
(deflanguage-parser-tests sparse-capture-test "sparse initial LR capture equals dense initial resume"
  (engine)
    (scenario "all checkpoint fields match at dense and sparse spacings"
      (scope ((source (apply string-append (make-list 64 "a=1\n"))))
        (each (spacing) ('(1 2 2.0 32 1024))
                    (equal (capture-observation source spacing #f)
                            (capture-observation source spacing #t)))))
    (scenario "empty and trivia-only sources retain exact initial boundaries"
      (each (source) ('("" " \t\n" "\r\n"))
                  (equal (capture-observation source 2 #f)
                          (capture-observation source 2 #t))))
    (scenario "Unicode strings and trivia retain modes and scan callback order"
      (each (source) ('("a = \"猫\"\n b=2\n" "a=1\r\nb=2\r\n" "a=1\n \t"))
                  (equal (capture-observation source 2 #f)
                          (capture-observation source 2 #t))))
    (scenario "rejection and incomplete EOF retain the dense diagnostics"
      (each (source) ('("a=" "a=1\n?" "a=\"unterminated" "a=1\n猫=2\n"))
                  (equal (capture-observation source 2 #f)
                          (capture-observation source 2 #t)))))
