;;; Performance gate lives in the Scheme/ASP test owner, outside the C product.
(import :std/test
        :asp-gerbil-scheme/src/benchmark/framework
        (only-in :asp-gerbil-scheme/src/benchmark/gate benchmark-run/result)
        (only-in :gerbil-parser/t/fixtures/progress report-test-progress!)
        (only-in :gerbil-parser/t/fixtures/native-ffi/language-v2-probe parse-native-batch parse-native-sized-batch create-native-handle)
        (only-in :gerbil-parser/src/ffi/language-handles release-native-language!))
(def native-language-benchmark-tests
  (test-suite "native language C performance"
    (test-case "scaled UTF-8 C calls preserve whole-call allocation observations"
      (let (handle (create-native-handle))
        (dynamic-wind
         void
         (lambda ()
           (for-each
            (lambda (shape)
              (let ((rows (car shape)) (unicode? (cadr shape)))
                (check (parse-native-sized-batch handle rows unicode? 1) => 0)
                (##gc)
                (let-values (((receipt admission-row)
                              (benchmark-run/result
                               (benchmark-contract-read "t/benchmarks/native-language/scaled-benchmark.ss")
                               (lambda ()
                                 (let* ((before (##process-statistics))
                                        (started (##current-time-point))
                                        (status (parse-native-sized-batch handle rows unicode? 1))
                                        (wall-ms (* 1000 (- (##current-time-point) started)))
                                        (after (##process-statistics))
                                        (delta (lambda (index)
                                                 (- (f64vector-ref after index)
                                                    (f64vector-ref before index))))
                                        (row (list (cons 'rows rows) (cons 'unicode? unicode?)
                                                   (cons 'source-bytes (* rows (if unicode? 6 4)))
                                                   (cons 'calls 1) (cons 'wall-ms wall-ms)
                                                   (cons 'cpu-ms (* 1000 (+ (delta 0) (delta 1))))
                                                   (cons 'gc-count (delta 6))
                                                   (cons 'gc-wall-ms (* 1000 (delta 5)))
                                                   (cons 'allocated-bytes (delta 7)))))
                                   (unless (zero? status) (error "scaled native batch rejected"))
                                   (report-test-progress! "LANGUAGE-ABI-SCALE " row)
                                   row)))))
                  (report-test-progress! "LANGUAGE-ABI-SCALE-ADMISSION " admission-row)
                  (check (benchmark-contract-receipt-pass? receipt) => #t))))
            '((256 #f) (4096 #f) (4096 #t))))
         (lambda () (release-native-language! handle)))))
    (test-case "one hundred warm C calls satisfy the ASP contract"
      (let (handle (create-native-handle))
        (check (positive? handle) => #t)
        (check (parse-native-batch handle) => 0)
        (##gc)
        (let (sample 0)
          (let-values (((receipt admission-row)
                        (benchmark-run/result
                         (benchmark-contract-read "t/benchmarks/native-language/benchmark.ss")
                         (lambda ()
                           (let* ((before (##process-statistics))
                                  (started (##current-time-point))
                                  (status (parse-native-batch handle))
                                  (wall-ms (* 1000 (- (##current-time-point) started)))
                                  (after (##process-statistics))
                                  (delta (lambda (index)
                                           (- (f64vector-ref after index)
                                              (f64vector-ref before index)))))
                             (unless (zero? status) (error "native batch rejected"))
                             ;; Preserve each observation. Independently ranked CPU
                             ;; percentiles cannot explain the wall P95 sample.
                             (let (row (list (cons 'sample sample)
                                              (cons 'wall-ms wall-ms)
                                              (cons 'cpu-ms (* 1000 (+ (delta 0) (delta 1))))
                                              (cons 'gc-count (delta 6))
                                              (cons 'gc-wall-ms (* 1000 (delta 5)))
                                              (cons 'allocated-bytes (delta 7))))
                               (set! sample (+ sample 1))
                               (report-test-progress! "LANGUAGE-ABI-BENCHMARK-BATCH calls=" 100
                                                      " observation=" row)
                               row))))))
          ;; This is the actual ASP-admitted wall-ranked attempt, preserving
          ;; its inner C batch counters even when progress output affects rank.
          (report-test-progress! "LANGUAGE-ABI-ADMISSION-SAMPLE " admission-row)
          (write receipt) (newline) (force-output)
          (check (benchmark-contract-receipt-pass? receipt) => #t)))
        (release-native-language! handle)))))
(export native-language-benchmark-tests)

(def native-language-benchmark-test native-language-benchmark-tests)
(export native-language-benchmark-test)
