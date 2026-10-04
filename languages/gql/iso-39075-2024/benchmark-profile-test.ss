;;; -*- Gerbil -*-
;;; Profiling must preserve lossless Unicode artifacts and LR token admission.
(import :std/test
        (only-in "./benchmarks/runtime/matched-stages" profile-gql-stages sample-at-percentile))
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
