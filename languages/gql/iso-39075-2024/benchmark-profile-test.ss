;;; -*- Gerbil -*-
;;; Profiling must preserve lossless Unicode artifacts and LR token admission.
(import :std/test
        (only-in "./parser" +gql-representative-query+)
        (only-in "./benchmarks/runtime/reduction-counts" profile-gql-reductions)
        (only-in "./benchmarks/runtime/matched-stages"
                 profile-gql-stages sample-at-percentile
                 gc-statistics-snapshot sample-gc-snapshot))
(export gql-benchmark-profile-test)
(def gql-benchmark-profile-test
  (test-suite "GQL language performance diagnostics"
    (test-case "Unicode and trivia retain exact artifacts through all stages"
      (let (reports (profile-gql-stages "RETURN '你好' /* retained */\n" 2 2))
        (check (map (lambda (row) (cdr (assq 'stage row))) reports)
               => '(global-lexing prepared-lr artifact-publication full-source))
        (for-each
         (lambda (row)
           (check (cdr (assq 'sampleCount row)) => 2)
           (check (cdr (assq 'parsesPerSample row)) => 2)
           (check (length (cdr (assq 'samples row))) => 2)) reports)))
    (test-case "selected derivation counts expose reduction and action costs"
      (let (counts (profile-gql-reductions +gql-representative-query+))
        (check (cdr (assq 'scope counts)) => 'selected-derivation)
        (check (cdr (assq 'reductions counts)) => 288)
        (check (cdr (assq 'concatenatingReductions counts)) => 75)
        (check (+ (cdr (assq 'emptyReductions counts))
                  (cdr (assq 'unaryReductions counts))
                  (cdr (assq 'multiOperandReductions counts))) => 288)
        (check (> (cdr (assq 'unaryReductions counts)) 0) => #t)
        (check (cdr (assq 'identityOperands counts)) => 192)
        (check (cdr (assq 'decoratedOperands counts)) => 132))
      (check-exception (profile-gql-reductions "RETURN @") true))
    (test-case "wall percentile retains its own CPU and GC observation"
      (let* ((rows (map (lambda (sample)
                          (list (cons 'sample sample) (cons 'wall-ms sample)
                                (cons 'cpu-ms (- 21 sample))
                                (cons 'gc-wall-ms (if (= sample 19) 11 0))))
                        (iota 20 1)))
             (tail (sample-at-percentile (reverse rows) 'wall-ms 95)))
        (check (cdr (assq 'sample tail)) => 19)
        (check (cdr (assq 'cpu-ms tail)) => 2)
        (check (cdr (assq 'gc-wall-ms tail)) => 11)))
    (test-case "GC snapshots retain VM live heap and distinguish stale collections"
      (let ((before (make-f64vector 20 0.0))
            (after (make-f64vector 20 0.0)))
        (f64vector-set! before 6 4.0)
        (f64vector-set! after 6 4.0)
        (f64vector-set! after 12 0.002)
        (f64vector-set! after 13 0.001)
        (f64vector-set! after 14 0.009)
        (f64vector-set! after 15 8192.0)
        (f64vector-set! after 16 6144.0)
        (f64vector-set! after 17 2048.0)
        (f64vector-set! after 18 1536.0)
        (f64vector-set! after 19 512.0)
        (check (sample-gc-snapshot before after) => #f)
        ;; Multiple collections still expose only the last one's snapshot.
        (f64vector-set! after 6 6.0)
        (let (snapshot (sample-gc-snapshot before after))
          (check snapshot => (gc-statistics-snapshot after))
          (check (cdr (assq 'scope snapshot)) => 'whole-vm-latest-collection)
          (check (cdr (assq 'gcCount snapshot)) => 6.0)
          (check (cdr (assq 'cpuMs snapshot)) => 3.0)
          (check (cdr (assq 'wallMs snapshot)) => 9.0)
          (check (cdr (assq 'heapBytes snapshot)) => 8192.0)
          (check (cdr (assq 'allocatedBytes snapshot)) => 6144.0)
          (check (cdr (assq 'liveBytes snapshot)) => 2048.0)
          (check (cdr (assq 'movableBytes snapshot)) => 1536.0)
          (check (cdr (assq 'stillBytes snapshot)) => 512.0))))
    (test-case "equal wall observations use sample identity for tie breaking"
      (let (rows '(((sample . 2) (wall-ms . 1))
                   ((sample . 0) (wall-ms . 1))
                   ((sample . 1) (wall-ms . 1))))
        (check (cdr (assq 'sample (sample-at-percentile rows 'wall-ms 50))) => 1)
        (check (cdr (assq 'sample (sample-at-percentile rows 'wall-ms 100))) => 2)))
    (test-case "rejected inputs cannot publish misleading stage receipts"
      (check-exception (profile-gql-stages "RETURN @" 2 2) true))
    (test-case "invalid measurement sizes fail before work"
      (check-exception (profile-gql-stages "RETURN 1" 0 2) true))))
