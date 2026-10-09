;;; Prepared shared-engine reductions; scanner and grammar construction excluded.
(import (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 lr-prepare lr-parse/prepared lr-parse/prepared/receipt
                 current-lr-event-program-enabled? lr-runtime-for-current-semantic-backend)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/src/runtime/artifact
                 make-success-parse-artifact parse-artifact-valid-for-source?)
        (only-in :gerbil-parser/src/runtime/parse-cost admit-parser-allocation))
(export benchmark-semantic-stack)

(def (benchmark-semantic-stack (samples 20) (iterations 100))
  (unless (and (integer? samples) (positive? samples)
               (integer? iterations) (positive? iterations))
    (error "semantic stack benchmark requires positive sample and iteration counts"))
  (for-each
   (lambda (width)
     (for-each
      (lambda (decorated?)
        (let* ((operand (if decorated?
                         '(field outer (alias Renamed (field inner (token word))))
                         '(token word)))
               (rules (list (list 'source-file (list 'alias 'SourceFile
                              (cons 'sequence (make-list width operand))))))
               (runtime (lr-prepare (compile-lr-spec rules 'source-file)))
               (tokens (map (lambda (index) (make-token 'word "a" index (+ index 1)))
                            (iota width)))
               (source (make-string width #\a))
               (digest (string-append "sha256:" (make-string 64 #\0))))
          (let-values (((reference rest receipt) (lr-parse/prepared/receipt runtime tokens)))
            (let (expected (make-success-parse-artifact digest source tokens reference false))
              (unless (and (null? rest) (parse-artifact-valid-for-source? expected source))
                (error "invalid independent stack reference" width decorated?))
              (for-each
               (lambda (events?)
                 (parameterize ((current-lr-event-program-enabled? events?))
                   (let (selected (lr-runtime-for-current-semantic-backend runtime))
                     (def (run publish?)
                       (let-values (((root rest) (lr-parse/prepared selected tokens)))
                         (unless (null? rest) (error "stack benchmark did not complete"))
                         (if publish?
                           (make-success-parse-artifact digest source tokens root false)
                           root)))
                     (unless (equal? (run #t) expected)
                       (error "stack benchmark changed complete product" width decorated? events?))
                     (for-each
                      (lambda (publish?)
                        (let warm ((count 0))
                          (when (< count iterations) (run publish?) (warm (+ count 1))))
                        (let sample ((index 0))
                          (when (< index samples)
                            (let (before (##process-statistics))
                              (let repeat ((count 0))
                                (when (< count iterations) (run publish?) (repeat (+ count 1))))
                              (let* ((after (##process-statistics))
                                     (delta (lambda (slot) (- (f64vector-ref after slot)
                                                             (f64vector-ref before slot)))))
                                (write (list 'semantic-stack width decorated? events? publish? index
                                         'cpu-ms-per-call (/ (* 1000 (+ (delta 0) (delta 1))) iterations)
                                         'allocated-bytes-per-call
                                         (let (bytes (admit-parser-allocation (delta 7) (delta 6)))
                                           (and bytes (/ bytes iterations)))))
                                (newline) (force-output)))
                            (sample (+ index 1)))))
                      '(#f #t)))))
               '(#f #t))))))
      '(#f #t)))
   '(1 8 32))
  (displayln "SEMANTIC-STACK-OK") (force-output))
