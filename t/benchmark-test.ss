#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :asp-gerbil-scheme/src/benchmark/framework
        :gerbil-parser/src/runtime/artifact
        :gerbil-parser/src/runtime/parser
        :gerbil-parser/languages/arithmetic/v1/parser)

(def benchmark-path "t/benchmarks/parser-hot-path/benchmark.ss")

(def (parse-batch)
  (let loop ((remaining 1000))
    (unless (zero? remaining)
      (unless
       (parse-artifact-success?
        (parse-source arithmetic-parser "1 + 2 * value - 3 / 4"))
       (error "benchmark parse failed"))
      (loop (- remaining 1))))
  ;; Emit only after a real batch completes. The measured thunk includes this
  ;; output cost, so the unchanged wall-time ceiling remains conservative.
  (displayln "BENCHMARK-BATCH-OK parses=1000")
  (force-output))

(def parser-benchmark-tests
  (test-suite "standard parser benchmark"
    (test-case "generated hot path satisfies ASP Gerbil benchmark contract"
      (check (benchmark-contract-valid? benchmark-path) => #t)
      ;; Calibrate the Gambit nursery before measurement, then collect outside
      ;; the timed action. Collections caused by the measured batch still count.
      (parse-batch)
      (##gc)
      (let (receipt
            (benchmark-contract-run
             benchmark-path
             parse-batch))
        (write receipt)
        (newline)
        (force-output)
        (check (benchmark-contract-receipt-pass? receipt) => #t)))))

(def benchmark-test parser-benchmark-tests)
(export parser-benchmark-tests benchmark-test)
