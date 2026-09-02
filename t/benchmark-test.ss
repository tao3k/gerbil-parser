#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :asp-gerbil-scheme/src/benchmark/framework
        :gerbil-parser/src/runtime/parser
        :gerbil-parser/src/reference/arithmetic)

(def benchmark-path "t/benchmarks/parser-hot-path/benchmark.ss")

(def parser-benchmark-tests
  (test-suite "standard parser benchmark"
    (test-case "generated hot path satisfies ASP Gerbil benchmark contract"
      (check (benchmark-contract-valid? benchmark-path) => #t)
      (let (receipt
            (benchmark-contract-run
             benchmark-path
             (lambda ()
               (let loop ((remaining 1000))
                 (unless (zero? remaining)
                   (unless
                    (parse-success?
                     (parse-source arithmetic-parser
                                   "1 + 2 * value - 3 / 4"))
                    (error "benchmark parse failed"))
                   (loop (- remaining 1)))))))
        (write receipt)
        (newline)
        (check (benchmark-contract-receipt-pass? receipt) => #t)))))

(run-tests! parser-benchmark-tests)
