(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-
;;; Profiling must preserve lossless Unicode artifacts and LR token admission.
(import :std/test
        (only-in :gerbil-parser/languages/gql/parser +gql-representative-query+)
        (only-in "../benchmarks/gql/runtime/reduction-counts" profile-gql-reductions)
        (only-in "../benchmarks/gql/runtime/execution-counts" profile-gql-prepared-execution)
        (only-in "../benchmarks/gql/runtime/matched-stages" profile-gql-stages)
        (only-in "../benchmarks/parser-stage-cost/benchmark"
                 sample-at-percentile measure-parser-batch
                 gc-statistics-snapshot sample-gc-snapshot
                 component-allocation-summary)
        (only-in :gerbil-parser/src/runtime/parse-cost admit-parser-allocation))

(deflanguage-parser-tests gql-benchmark-profile-test "GQL language performance diagnostics"
  (engine)
    (scenario "timed batches reject truncation and changed products"
      (sequential ((calls 0)
             (thunk (lambda () (set! calls (+ calls 1)) 'complete))
             (row (measure-parser-batch 'shared-control 7 4 thunk 'complete 2)))
        (equal calls  2)
        (equal (cdr (assq 'sample row))  7)
        (each (sizes) ('((0 1) (3 2) (4 0) (4.0 1)))
           (raises
            (measure-parser-batch 'invalid 0 (car sizes) thunk 'complete (cadr sizes)) true))
        (equal calls  2)
        (raises
         (measure-parser-batch 'changed 0 1 (lambda () 'wrong) 'complete) true)))
    (scenario "allocation summaries exclude GC intervals and retain unavailable ownership"
      (sequential ((rows
              (map (lambda (observation)
                     (list (cons 'allocated-bytes
                                 (admit-parser-allocation (car observation) (cadr observation)))))
                   '((1000 0) (2000 0) (999999 1) (-100 0))))
             (summary (component-allocation-summary rows 10 'single-caller)))
        (equal (cdr (assq 'allocationSampleCount summary))  2)
        (equal (cdr (assq 'allocatedBytesPerParse summary))  100)
        (equal (cdr (assq 'allocationP95BytesPerParse summary))  200)
        (each (unavailable) ((list (component-allocation-summary rows 10 'process-with-thread-switches)
               (component-allocation-summary '(((allocated-bytes . #f))) 10 'single-caller)))
           (equal (cdr (assq 'allocationSampleCount unavailable))  0)
           (equal (cdr (assq 'allocatedBytesPerParse unavailable))  #f)
           (equal (cdr (assq 'allocationP95BytesPerParse unavailable))  #f))
        (equal (cdr (assq 'allocatedBytesPerParse
                         (component-allocation-summary '(((allocated-bytes . 0))) 10 'single-caller)))
                0)))
    (scenario "Unicode and trivia retain exact artifacts through all stages"
      (scope ((reports (profile-gql-stages "RETURN '你好' /* retained */\n" 2 2)))
        (equal (map (lambda (row) (cdr (assq 'stage row))) reports)
                '(global-lexing prepared-lr artifact-publication full-source))
        (each (row) (reports)
           (equal (cdr (assq 'sampleCount row))  2)
           (equal (cdr (assq 'parsesPerSample row))  2)
           (equal (length (cdr (assq 'samples row)))  2))))
    (scenario "selected derivation counts expose reduction and action costs"
      (scope ((counts (profile-gql-reductions +gql-representative-query+)))
        (equal (cdr (assq 'scope counts))  'selected-derivation)
        (equal (cdr (assq 'reductions counts))  288)
        (equal (cdr (assq 'concatenatingReductions counts))  75)
        (equal (+ (cdr (assq 'emptyReductions counts))
                  (cdr (assq 'unaryReductions counts))
                  (cdr (assq 'multiOperandReductions counts)))  288)
        (equal (> (cdr (assq 'unaryReductions counts)) 0)  #t)
        (equal (cdr (assq 'identityOperands counts))  192)
        (equal (cdr (assq 'decoratedOperands counts))  132))
      (raises (profile-gql-reductions "RETURN @") true))
    (scenario "stepped executor accounts for actions and preserves directed artifacts"
      (scope ((counts (profile-gql-prepared-execution +gql-representative-query+)))
        (equal (cdr (assq 'scope counts))  'prepared-lr-single-action-checkpoints)
        (equal (cdr (assq 'shifts counts))  37)
        (equal (cdr (assq 'reductions counts))  288)
        (equal (cdr (assq 'actions counts))  325)
        (equal (cdr (assq 'accepts counts))  1)
        (equal (cdr (assq 'literalFreeNonEofObservations counts))  0)
        (equal (cdr (assq 'caseFoldObservations counts))  21)
        (equal (cdr (assq 'caseFoldTokens counts))  14)
        (equal (cdr (assq 'repeatedCaseFoldObservations counts))  7)
        (equal (+ (cdr (assq 'nonEofObservations counts))
                  (cdr (assq 'eofObservations counts)))  326))
      (equal (cdr (assq 'accepts (profile-gql-prepared-execution "match (n) return n")))  1)
      (scope ((upper (profile-gql-prepared-execution "MATCH (n) RETURN n"))
            (lower (profile-gql-prepared-execution "match (n) return n")))
        ;; Exact literal hits short-circuit folding; lowercase contextual
        ;; keywords add conversion observations without changing shifts.
        (equal (cdr (assq 'shifts upper))  (cdr (assq 'shifts lower)))
        (equal (> (cdr (assq 'caseFoldObservations lower))
                  (cdr (assq 'caseFoldObservations upper)))  #t))
      (raises (profile-gql-prepared-execution "RETURN @") true))
    (property "wall percentile retains its own CPU and GC observation"
      (bindings (rows (map (lambda (sample)
                          (list (cons 'sample sample) (cons 'wall-ms sample)
                                (cons 'cpu-ms (- 21 sample))
                                (cons 'gc-wall-ms (if (= sample 19) 11 0))))
                        (iota 20 1))) (tail (sample-at-percentile (reverse rows) 'wall-ms 95)))
      (equal (cdr (assq 'sample tail))  19)
      (equal (cdr (assq 'cpu-ms tail))  2)
      (equal (cdr (assq 'gc-wall-ms tail))  11))
    (scenario "GC snapshots retain VM live heap and distinguish stale collections"
      (scope ((before (make-f64vector 20 0.0))
            (after (make-f64vector 20 0.0)))
        (act (f64vector-set! before 6 4.0))
        (act (f64vector-set! after 6 4.0))
        (act (f64vector-set! after 12 0.002))
        (act (f64vector-set! after 13 0.001))
        (act (f64vector-set! after 14 0.009))
        (act (f64vector-set! after 15 8192.0))
        (act (f64vector-set! after 16 6144.0))
        (act (f64vector-set! after 17 2048.0))
        (act (f64vector-set! after 18 1536.0))
        (act (f64vector-set! after 19 512.0))
        (equal (sample-gc-snapshot before after)  #f)
        ;; Multiple collections still expose only the last one's snapshot.
        (act (f64vector-set! after 6 6.0))
        (scope ((snapshot (sample-gc-snapshot before after)))
          (equal snapshot  (gc-statistics-snapshot after))
          (equal (cdr (assq 'scope snapshot))  'whole-vm-latest-collection)
          (equal (cdr (assq 'gcCount snapshot))  6.0)
          (equal (cdr (assq 'cpuMs snapshot))  3.0)
          (equal (cdr (assq 'wallMs snapshot))  9.0)
          (equal (cdr (assq 'heapBytes snapshot))  8192.0)
          (equal (cdr (assq 'allocatedBytes snapshot))  6144.0)
          (equal (cdr (assq 'liveBytes snapshot))  2048.0)
          (equal (cdr (assq 'movableBytes snapshot))  1536.0)
          (equal (cdr (assq 'stillBytes snapshot))  512.0))))
    (property "equal wall observations use sample identity for tie breaking"
      (bindings (rows '(((sample . 2) (wall-ms . 1))
                   ((sample . 0) (wall-ms . 1))
                   ((sample . 1) (wall-ms . 1)))))
      (equal (cdr (assq 'sample (sample-at-percentile rows 'wall-ms 50)))  1)
      (equal (cdr (assq 'sample (sample-at-percentile rows 'wall-ms 100)))  2))
    (scenario "rejected inputs cannot publish misleading stage receipts"
      (raises (profile-gql-stages "RETURN @" 2 2) true))
    (scenario "invalid measurement sizes fail before work"
      (raises (profile-gql-stages "RETURN 1" 0 2) true)))
