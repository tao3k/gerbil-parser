;;; Native timing evidence for the unchanged 100-parse/40-sample workload.
;;; Diagnostic only: does not replace the benchmark's wall-clock gate.
(import (only-in :gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process
                 test-child-process-exit!)
        :std/ffi
        (only-in :gerbil-parser/languages/gql/parser
                 parse-gql)
        (only-in :gerbil-parser/languages/gql/parser
                 +gql-representative-query+)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-success?))
(export main)
(C-include "<time.h>" "<sys/resource.h>")
(def-C-lambda (monotonic-seconds) => double
  "struct timespec ts;
   if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) ___result = -1.0;
   else ___result = ts.tv_sec + ts.tv_nsec * 1e-9;")

(def-C-lambda (voluntary-switches) => long
  "struct rusage usage;
   if (getrusage(RUSAGE_SELF, &usage) != 0) ___result = -1;
   else ___result = usage.ru_nvcsw;")
(def-C-lambda (involuntary-switches) => long
  "struct rusage usage;
   if (getrusage(RUSAGE_SELF, &usage) != 0) ___result = -1;
   else ___result = usage.ru_nivcsw;")

(def (parse-batch)
  (let loop ((remaining 100))
    (unless (zero? remaining)
      (unless (parse-artifact-success?
               (parse-gql +gql-representative-query+))
        (error "native timing query rejected"))
      (loop (- remaining 1)))))

(def (main . _)
  (when (< (monotonic-seconds) 0) (error "monotonic clock unavailable"))
  (parse-batch)
  (##gc)
  (let loop ((sample 0) (elapsed '()))
    (if (= sample 40)
      (begin
        (write (list 'GQL-NATIVE-TIMING-OK 'samples 40 'parses-per-sample 100
                     'monotonic-max-ms (apply max elapsed)))
        (newline) (force-output)
        (test-child-process-exit! 0))
      (let* ((before (##process-statistics))
             (voluntary (voluntary-switches))
             (involuntary (involuntary-switches))
             (wall (time->seconds (current-time)))
             (mono (monotonic-seconds)))
        (parse-batch)
        (let* ((mono-ms (* 1000 (- (monotonic-seconds) mono)))
               (wall-ms (* 1000 (- (time->seconds (current-time)) wall)))
               (after (##process-statistics))
               (delta (lambda (index)
                        (- (f64vector-ref after index)
                           (f64vector-ref before index)))))
          ;; Gambit process-statistics indices are runtime cumulative counters.
          (write (list 'GQL-NATIVE-SAMPLE sample 'monotonic-ms mono-ms
                       'wall-ms wall-ms 'cpu-ms (* 1000 (+ (delta 0) (delta 1)))
                       'gc-count (delta 6) 'gc-wall-ms (* 1000 (delta 5))
                       'allocated-bytes (delta 7)
                       'minor-faults (delta 10) 'major-faults (delta 11)
                       'voluntary-switches (- (voluntary-switches) voluntary)
                       'involuntary-switches (- (involuntary-switches) involuntary)))
          (newline) (force-output)
          (loop (+ sample 1) (cons mono-ms elapsed)))))))
