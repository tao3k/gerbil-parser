#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
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

(deflanguage-parser-tests parser-benchmark-tests "standard parser benchmark"
  (engine (exclusive #t))
    (scenario "paired observations conserve requests and balance quadratic leg position"
      (each (calls) ('(1 2 3 25 256 768))
         (each (group) ('(0 1))
            (scope ((plan (cpu-pair-request-plan calls group)))
              (equal (length plan)  8)
              (equal (andmap (lambda (work) (and (exact-integer? (cdr work)) (positive? (cdr work)))) plan)  #t)
              (each (variant) ('(left right))
                 (equal (apply + (map cdr (filter (lambda (work) (eq? (car work) variant)) plan)))  (* 4 calls)))
              (each (degree) ('(0 1 2))
                 (equal
                  (apply +
                    (map (lambda (work index)
                           (* (if (eq? (car work) 'left) 1 -1) (cdr work) (expt index degree)))
                         plan (iota (length plan))))  0))))))
    (scenario "paired sampler executes every complete request in its declared budget"
      (sequential ((source "1 + 2 * value - 3 / 4")
             (expected (parse-source arithmetic-parser source))
             (left-count 0) (right-count 0))
        (act (measure-parser-cpu-pairs 'request-budget 'control 20 256 expected
          (lambda () (set! left-count (+ left-count 1)) (parse-source arithmetic-parser source))
          (lambda () (set! right-count (+ right-count 1)) (parse-source arithmetic-parser source))))
        ;; Each side owns one validated warmup plus twenty complete groups.
        (equal left-count  (+ 1 (* 20 4 256)))
        (equal right-count  (+ 1 (* 20 4 256)))))
    (scenario "generated hot path satisfies ASP Gerbil benchmark contract"
      (equal (benchmark-contract-valid? benchmark-path)  #t)
      ;; Calibrate the Gambit nursery before measurement, then collect outside
      ;; the timed action. Collections caused by the measured batch still count.
      (act (parse-batch))
      (act (##gc))
      (scope ((receipt
            (benchmark-contract-run
             benchmark-path
             parse-batch)))
        (receipt receipt)
        (equal (benchmark-contract-receipt-pass? receipt)  #t))))

(def benchmark-test parser-benchmark-tests)
(export benchmark-test)

;; Admit wall/CPU contracts after all shared-runtime semantic owners join.
