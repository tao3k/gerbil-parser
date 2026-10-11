#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :asp-gerbil-scheme/src/benchmark/framework
        :gerbil-parser/src/runtime/artifact
        :gerbil-parser/src/runtime/parser
        :gerbil-parser/languages/arithmetic/parser
        (only-in :gerbil-parser/t/benchmarks/parser-stage-cost/benchmark cpu-pair-request-plan measure-parser-cpu-pairs)
        (only-in :gerbil-parser/t/fixtures/progress report-parser-batch!))

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
  (report-parser-batch! 1000))

(def parser-benchmark-tests
  (test-suite "standard parser benchmark"
    (test-case "paired observations conserve requests and balance quadratic leg position"
      (for-each
       (lambda (calls)
         (for-each
          (lambda (group)
            (let (plan (cpu-pair-request-plan calls group))
              (check (andmap (lambda (work) (and (exact-integer? (cdr work)) (positive? (cdr work)))) plan) => #t)
              (for-each
               (lambda (variant)
                 (check (apply + (map cdr (filter (lambda (work) (eq? (car work) variant)) plan))) => (* 4 calls)))
               '(left right))
              (for-each
               (lambda (degree)
                 (check
                  (apply +
                    (map (lambda (work index)
                           (* (if (eq? (car work) 'left) 1 -1) (cdr work) (expt index degree)))
                         plan (iota (length plan)))) => 0))
               '(0 1 2)))) '(0 1)))
       '(1 2 3 25 256 768)))
    (test-case "paired sampler executes every complete request in its declared budget"
      (let* ((source "1 + 2 * value - 3 / 4")
             (expected (parse-source arithmetic-parser source))
             (left-count 0) (right-count 0))
        (measure-parser-cpu-pairs 'request-budget 'control 20 256 expected
          (lambda () (set! left-count (+ left-count 1)) (parse-source arithmetic-parser source))
          (lambda () (set! right-count (+ right-count 1)) (parse-source arithmetic-parser source)))
        ;; Each side owns one validated warmup plus twenty complete groups.
        (check left-count => (+ 1 (* 20 4 256)))
        (check right-count => (+ 1 (* 20 4 256)))))
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
