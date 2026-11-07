;;; Diagnostic only. Read the installed Gambit header's VM GC counters;
;;; never alter the collector or scheduler. Per-request probes add overhead.
(import :std/ffi
        (only-in :std/os/time current-system-time-precise timespec-tv_sec timespec-tv_nsec)
        :gerbil-parser/benchmarks/real-fold/parse-org-compiled-events)
(export main)
(C-ffi-macrology)
(C-include "<sys/resource.h>")
(C-include "<time.h>")
;; Independent monotonic clock checks realtime sampling, not the collector.
(def-C-lambda monotonic-us () double
  "struct timespec t; if (clock_gettime(CLOCK_MONOTONIC, &t)) ___return(-1.0);
   ___return(t.tv_sec * 1e6 + t.tv_nsec * 1e-3);")
(def-C-lambda gc-real () double "___return(___VMSTATE->mem.gc_real_time_);")
(def-C-lambda gc-cpu () double
  "___return(___VMSTATE->mem.gc_user_time_ + ___VMSTATE->mem.gc_sys_time_);")
(def-C-lambda gc-count () double "___return(___VMSTATE->mem.nb_gcs_);")
(def-C-lambda cpu () double
  "struct rusage r; if (getrusage(RUSAGE_SELF, &r)) ___return(-1.0);
   ___return(r.ru_utime.tv_sec + r.ru_stime.tv_sec +
             (r.ru_utime.tv_usec + r.ru_stime.tv_usec) * 1e-6);")
(def-C-lambda switches () long
  "struct rusage r; if (getrusage(RUSAGE_SELF, &r)) ___return(-1);
   ___return(r.ru_nivcsw);")
(def-C-lambda voluntary-switches () long
  "struct rusage r; if (getrusage(RUSAGE_SELF, &r)) ___return(-1);
   ___return(r.ru_nvcsw);")
(def (clock-us)
  (let (t (current-system-time-precise))
    (+ (* (timespec-tv_sec t) 1000000) (quotient (timespec-tv_nsec t) 1000))))
(def (main corpus-path (mode "stalls"))
  (let* ((corpus (call-with-input-file corpus-path read))
         (inputs (caddr corpus)) (expected (cadddr corpus)) (rows '())
         (comparison-wall-us 0) (batch-wall 0) (batch-cpu 0))
    (unless (and (eq? (car corpus) 'real-fold-preflight.v1)
                 (= (vector-length inputs) 1000) (= (vector-length expected) 1000))
      (error "invalid diagnostic corpus"))
    (unless (member mode '("stalls" "profile")) (error "unknown diagnostic mode" mode))
    (when (equal? mode "profile")
      (displayln "FOLD-PROFILE-READY") (force-output)
      ;; Keep one AOT process alive for the OS sampler, not a timed matrix.
      ;; Sampled stack owners distinguish complete tape comparison from Fold.
      (for-each
        (lambda (_batch)
          (for-each
            (lambda (index)
              (unless (equal? (parse-org-compiled-events (vector-ref inputs index))
                              (vector-ref expected index))
                (error "profile tape mismatch" index)))
            (iota 1000)))
        (iota 100))
      (displayln "FOLD-PROFILE-OK complete tapes; NOT PERFORMANCE ACCEPTANCE")
      (exit 0))
    (set! batch-cpu (cpu))
    (set! batch-wall (monotonic-us))
    (for-each
      (lambda (index)
        (let* ((c0 (cpu)) (g0 (gc-real)) (gc0 (gc-cpu))
               (n0 (gc-count)) (s0 (switches)) (v0 (voluntary-switches))
               (m0 (monotonic-us)) (t0 (clock-us))
               (result (parse-org-compiled-events (vector-ref inputs index)))
               (wall (- (clock-us) t0)) (c1 (cpu))
               (m1 (monotonic-us))
               (g1 (gc-real)) (gc1 (gc-cpu)) (n1 (gc-count)) (s1 (switches))
               (v1 (voluntary-switches)))
          (let (comparison-start (monotonic-us))
            (unless (equal? result (vector-ref expected index))
              (error "diagnostic tape mismatch" index))
            (set! comparison-wall-us
                  (+ comparison-wall-us (- (monotonic-us) comparison-start))))
          (set! rows (cons (list wall index (* 1000000 (- c1 c0))
                                (* 1000000 (- g1 g0)) (* 1000000 (- gc1 gc0))
                                (- n1 n0) (- s1 s0) (- v1 v0) (- m1 m0)) rows))))
      (iota 1000))
    (set! batch-wall (- (monotonic-us) batch-wall))
    (set! batch-cpu (* 1000000 (- (cpu) batch-cpu)))
    (displayln "BATCH-DIAGNOSTIC wall-us=" batch-wall " cpu-us=" batch-cpu
               " parse-realtime-us=" (foldl + 0 (map car rows))
               " parse-monotonic-us=" (foldl + 0 (map (cut list-ref <> 8) rows))
               " comparison-wall-us=" comparison-wall-us)
    (displayln "DIAGNOSTIC-ONLY probes add overhead; NOT PERFORMANCE ACCEPTANCE")
    (displayln "FIELDS realtime-wall-us input-index cpu-us gc-wall-us gc-cpu-us gc-count involuntary-switches voluntary-switches monotonic-wall-us")
    (for-each (lambda (row) (write row) (newline))
      (take (list-sort (lambda (a b) (> (car a) (car b))) rows) 20))
    (displayln "FOLD-STALLS-OK complete tapes; historical spikes not retrospectively classified")))
