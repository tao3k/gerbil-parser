;;; Complete requests compare the prepared classifier with the independent
;;; original row selector. No measurement callback enters the timed executor.
(import (only-in :gerbil-parser/src/compiler/machine parser-machine-with-lr-action-selector
                 parser-machine-runtime)
        (only-in :gerbil-parser/src/runtime/parser parse-source)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-valid-for-source?)
        (only-in :gerbil-parser/src/runtime/parse-cost current-parser-cost-observer)
        (only-in :gerbil-parser/src/runtime/lr-parser lr-runtime-direct-step lr-runtime-layout?)
        (only-in :gerbil-parser/t/fixtures/lr-action-selection reference-lr-action-selector)
        (only-in :gerbil-parser/t/benchmarks/parser-stage-cost/benchmark
                 measure-parser-cpu-pairs measure-parser-component))
(export benchmark-lr-lookahead profile-lr-lookahead)

;;; Count original selection work outside timing. Counts establish reuse
;;; eligibility, not an additive CPU decomposition or a measured speedup.
(def (profile-lr-lookahead machine source expected)
  (let ((observations 0) (eof-observations 0) (lookaheads (make-table test: eq?)))
    (let* ((factory
            (lambda (rows index casefold? layout?)
              (let (select (reference-lr-action-selector rows index casefold? layout?))
                (lambda (state tokens)
                  (if (null? tokens)
                    (set! eof-observations (+ eof-observations 1))
                    (begin
                      (set! observations (+ observations 1))
                      (table-set! lookaheads (car tokens) #t)))
                  (select state tokens)))))
           (observed (parse-source (parser-machine-with-lr-action-selector machine factory) source)))
      (unless (equal? observed expected) (error "lookahead observation changes complete product"))
      (list (cons 'actionObservations observations)
            (cons 'lookaheads (table-length lookaheads))
            (cons 'eofObservations eof-observations)))))

(def (benchmark-lr-lookahead workload machine source (groups 20) (calls 100))
  (let* ((stages '())
         (witness
          (parameterize
           ((current-parser-cost-observer
             (lambda (name receipt)
               (when (cdr (assq 'completed receipt)) (set! stages (cons name stages))))))
           (parse-source machine source)))
         (checkpoint? (memq 'prepared-checkpoint-execution stages))
         (generated? (or (memq 'generated-drive-execution stages)
                         (and (memq 'generated-source-product stages) (not checkpoint?))))
         (runtime (parser-machine-runtime machine))
         (unchanged? (or generated? (lr-runtime-direct-step runtime) (lr-runtime-layout? runtime)))
         ;; An installed generated request is an identical-executor control.
         ;; Substituting a selector would remove its generated driver and
         ;; compare different executors rather than the proposed classifier.
         (reference (if unchanged? machine
                      (parser-machine-with-lr-action-selector machine reference-lr-action-selector)))
         (expected (parse-source reference source))
         (left (lambda () (parse-source reference source)))
         (right (lambda () (parse-source machine source))))
    (unless (and (or generated? checkpoint?) (equal? witness expected))
      (error "lookahead request lacks an equivalent executor witness" workload stages))
    (unless (parse-artifact-valid-for-source? expected source)
      (error "lookahead benchmark requires a source-bound complete product" workload))
    (write (list 'LR-LOOKAHEAD-WORK workload
                 (if unchanged? 'identical-executor-control
                   (profile-lr-lookahead machine source expected))))
    (newline) (force-output)
    (measure-parser-component (list workload 'original-request) 20 calls left expected)
    (measure-parser-component (list workload 'classified-request) 20 calls right expected)
    (let (ratios (measure-parser-cpu-pairs 'lr-lookahead workload groups calls expected left right))
      (write (list 'LR-LOOKAHEAD-CPU-RATIOS workload ratios))
      (newline) (force-output))))
