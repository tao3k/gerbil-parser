;;; Performance gate lives in the Scheme/ASP test owner, outside the C product.
(import :std/test
        :asp-gerbil-scheme/src/benchmark/framework
        (only-in :gerbil-parser/t/fixtures/progress report-test-progress!)
        (only-in :gerbil-parser/t/fixtures/native-ffi/language-v2-probe parse-native-batch create-native-handle)
        (only-in :gerbil-parser/src/ffi/language-handles release-native-language!))
(def native-language-benchmark-tests
  (test-suite "native language C performance"
    (test-case "one hundred warm C calls satisfy the ASP contract"
      (let (handle (create-native-handle))
        (check (positive? handle) => #t)
        (check (parse-native-batch handle) => 0)
        (##gc)
        (let (receipt (benchmark-contract-run "t/benchmarks/native-language/benchmark.ss"
                         (lambda ()
                           (unless (zero? (parse-native-batch handle)) (error "native batch rejected"))
                           (report-test-progress! "LANGUAGE-ABI-BENCHMARK-BATCH calls=" 100))))
          (write receipt) (newline) (force-output)
          (check (benchmark-contract-receipt-pass? receipt) => #t))
        (release-native-language! handle)))))
(export native-language-benchmark-tests)

(def native-language-benchmark-test native-language-benchmark-tests)
(export native-language-benchmark-test)
