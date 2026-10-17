(import :gerbil-parser/language-test-support)
;;; Protocol controls, not performance samples.
(import (only-in :std/test check check-exception test-case test-suite)
        (only-in ./benchmarks/parser-throughput/protocol
                 parser-throughput-percentile parser-throughput-gate
                 parser-throughput-batch run-parser-throughput))


(deflanguage-parser-tests parser-throughput-test "Parser throughput protocol"
  (engine)
    (scenario "nearest-rank uses request samples not batch totals"
      (scope ((samples (list->vector (iota 100 1))))
        (equal (parser-throughput-percentile samples 95)  95)
        (equal (parser-throughput-percentile samples 99)  99))
      (equal (parser-throughput-percentile '#(1 2 3) 95)  3)
      (raises (parser-throughput-percentile '#() 95) true)
      (raises (parser-throughput-percentile '#(1) 0) true))
    (scenario "wall gates scale to each declared load"
      (each (count) ('(1000 10000 100000))
                  (scope ((limit (/ count 100000)))
                    (equal (parser-throughput-gate count limit)  #t)
                    (equal (parser-throughput-gate count (+ limit 1/1000000))  #f)))
      (equal (parser-throughput-gate 100000 32)  #f)
      (equal (parser-throughput-gate 100000 0)  #f)
      (equal (parser-throughput-gate 0 1)  #f))
    (scenario "semantic mismatch cannot produce a passing sample"
      (raises (parser-throughput-batch (lambda (_) 'wrong)
                                               '#(changing) '#(expected)) true)
      (raises (parser-throughput-batch values '#(1) '#()) true))
    (scenario "reject unsupported modes before starting a matrix"
      (raises (run-parser-throughput "test" "sha256:test" values values values
                                              'partial-pass) true)
      (raises (run-parser-throughput "test" "" values values values 'full) true)))
