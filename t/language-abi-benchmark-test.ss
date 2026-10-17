(import :gerbil-parser/language-test-support)
;;; Performance gate lives in the Scheme/ASP test owner, outside the C product.
(import :std/test
        :asp-gerbil-scheme/src/benchmark/framework
        (only-in :asp-gerbil-scheme/src/benchmark/gate benchmark-run/result)
        (only-in :gerbil-parser/t/fixtures/progress report-test-progress!)
        (only-in :gerbil-parser/t/fixtures/ffi/language-probe parse-native-batch parse-native-sized-batch create-native-handle)
        (only-in :gerbil-parser/src/ffi/language-handles release-language!))
(deflanguage-parser-tests language-abi-benchmark-tests "native language C performance"
  (engine (exclusive #t) (affinity language-abi-runtime))
    (scenario "scaled UTF-8 C calls preserve whole-call allocation observations"
      (scope ((handle (create-native-handle)))
        (wind
          (before (act (void)))
          (body (each (shape) ('((256 #f) (4096 #f) (4096 #t)))
              (scope ((rows (car shape)) (unicode? (cadr shape)))
                (equal (parse-native-sized-batch handle rows unicode? 1)  0)
                (act (##gc))
                (scope-values (((receipt admission-row)
                              (benchmark-run/result
                               (benchmark-contract-read "t/benchmarks/language-abi/scaled-benchmark.ss")
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
                  (act (report-test-progress! "LANGUAGE-ABI-SCALE-ADMISSION " admission-row))
                  (equal (benchmark-contract-receipt-pass? receipt)  #t)))))
          (after (act (release-language! handle))))))
    (scenario "one hundred warm C calls satisfy the ASP contract"
      (scope ((handle (create-native-handle)))
        (equal (positive? handle)  #t)
        (equal (parse-native-batch handle)  0)
        (act (##gc))
        (scope ((sample 0))
          (scope-values (((receipt admission-row)
                        (benchmark-run/result
                         (benchmark-contract-read "t/benchmarks/language-abi/benchmark.ss")
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
          (act (report-test-progress! "LANGUAGE-ABI-ADMISSION-SAMPLE " admission-row))
          (receipt receipt)
          (equal (benchmark-contract-receipt-pass? receipt)  #t)))
        (act (release-language! handle)))))


(def language-abi-benchmark-test language-abi-benchmark-tests)
(export language-abi-benchmark-test)

;;; Both ABI Suites share the persistent runtime owner thread.


;; Resource admission owns its measurement interval in the shared runtime.
