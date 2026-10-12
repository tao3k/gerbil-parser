;;; Protocol controls, not performance samples.
(import (only-in :std/test check check-exception test-case test-suite)
        (only-in ./benchmarks/parser-throughput/protocol
                 parser-throughput-percentile parser-throughput-gate
                 parser-throughput-batch run-parser-throughput))
(export parser-throughput-test)

(def parser-throughput-test
  (test-suite "Parser throughput protocol"
    (test-case "nearest-rank uses request samples not batch totals"
      (let (samples (list->vector (iota 100 1)))
        (check (parser-throughput-percentile samples 95) => 95)
        (check (parser-throughput-percentile samples 99) => 99))
      (check (parser-throughput-percentile '#(1 2 3) 95) => 3)
      (check-exception (parser-throughput-percentile '#() 95) true)
      (check-exception (parser-throughput-percentile '#(1) 0) true))
    (test-case "wall gates scale to each declared load"
      (for-each (lambda (count)
                  (let (limit (/ count 100000))
                    (check (parser-throughput-gate count limit) => #t)
                    (check (parser-throughput-gate count (+ limit 1/1000000)) => #f)))
                '(1000 10000 100000))
      (check (parser-throughput-gate 100000 32) => #f)
      (check (parser-throughput-gate 100000 0) => #f)
      (check (parser-throughput-gate 0 1) => #f))
    (test-case "semantic mismatch cannot produce a passing sample"
      (check-exception (parser-throughput-batch (lambda (_) 'wrong)
                                               '#(changing) '#(expected)) true)
      (check-exception (parser-throughput-batch values '#(1) '#()) true))
    (test-case "reject unsupported modes before starting a matrix"
      (check-exception (run-parser-throughput "test" "sha256:test" values values values
                                              'partial-pass) true)
      (check-exception (run-parser-throughput "test" "" values values values 'full) true))))
