;;; One independent workload: checked construction of fresh GrammarRole/Grammar pairs.
(import (only-in :gerbil-parser/src/grammar/declaration-objects
                 make-grammar-role make-grammar grammar? grammar-roles grammar-role?)
        (only-in :asp-gerbil-scheme/src/benchmark/framework
                 benchmark-contract-valid? benchmark-contract-run
                 benchmark-contract-receipt-pass?))
(export make-observed-grammar-batch observed-grammar-batch-valid?
        run-grammar-construction-benchmark main)

(def benchmark-path "t/benchmarks/poo-grammar-objects/benchmark.ss")

(def (make-observed-role)
  (make-grammar-role
   'observability-role '() '() '() '() '() '() '() '() '()))

(def (make-observed-grammar role)
  (make-grammar 'observability-grammar '() (list role)))

(def (make-batch make-item)
  (let loop ((remaining 50) (items '()))
    (if (zero? remaining)
      items
      (loop (- remaining 1)
            (cons (make-item) items)))))

(def (make-observed-grammar-batch)
  (make-batch (lambda () (make-observed-grammar (make-observed-role)))))

(def (observed-grammar-batch-valid? grammars)
  (and (= (length grammars) 50)
       (andmap
        (lambda (grammar)
          (and (grammar? grammar)
               (= (length (grammar-roles grammar)) 1)
               (grammar-role? (car (grammar-roles grammar)))))
        grammars)))

(def (run-grammar-construction-benchmark)
  (unless (benchmark-contract-valid? benchmark-path)
    (error "invalid grammar construction benchmark contract" benchmark-path))
  (unless (observed-grammar-batch-valid? (make-observed-grammar-batch))
    (error "grammar construction warmup changed the workload result"))
  (##gc)
  ;; Retain process-wide counters in sample order. Both the batch and
  ;; the enclosing ASP interval are free of progress and diagnostic I/O.
  (let* ((samples '())
         (receipt
          (benchmark-contract-run benchmark-path
            (lambda ()
              (let (stats (##exec-stats make-observed-grammar-batch))
                (set! samples
                  (cons (map (lambda (key) (assq key stats))
                             '(user-time sys-time real-time gc-real-time
                               gc-user-time gc-sys-time nb-gcs bytes-allocated
                               minflt majflt))
                        samples))
                ;; The receipt consumes timing and counters. This batch has
                ;; completed checked construction, so its objects need not
                ;; remain live throughout the entire series.
                (void))))))
    (let report ((remaining (reverse samples)) (attempt 1))
      (unless (null? remaining)
        (write (cons 'CONSTRUCTION-DIAGNOSTIC
                     (cons (cons 'attempt attempt) (car remaining))))
        (newline)
        (report (cdr remaining) (+ attempt 1))))
    (force-output)
    receipt))

(def (report-phase phase workload valid?)
  (def (checked-workload)
    (let (result (workload))
      (unless (valid? result)
        (error "invalid grammar construction phase result" phase))))
  (checked-workload)
  (##gc)
  (let (samples
        (let collect ((attempt 1) (samples '()))
          (if (> attempt 20)
            (reverse samples)
            (let* ((result #f)
                   (stats (##exec-stats (lambda () (set! result (workload))))))
              (unless (valid? result)
                (error "invalid grammar construction phase sample" phase attempt))
              (collect (+ attempt 1)
                       (cons (list phase attempt stats) samples))))))
    (for-each (lambda (sample)
                (write (cons 'CONSTRUCTION-PHASE sample))
                (newline))
              samples)
    (force-output)))

(def (run-construction-gate)
  (let (receipt (run-grammar-construction-benchmark))
    (write receipt) (newline) (force-output)
    (unless (benchmark-contract-receipt-pass? receipt)
      (error "grammar construction benchmark exceeded its declared budget"))
    (displayln "GRAMMAR-CONSTRUCTION-BENCHMARK-OK") (force-output)))

(def (main . args)
  (match args
    ([] (run-construction-gate))
    (["role"]
     (report-phase 'role
                   (lambda () (make-batch make-observed-role))
                   (lambda (roles)
                     (and (= (length roles) 50) (andmap grammar-role? roles)))))
    (["assembly"]
     (let (roles (make-batch make-observed-role))
       (report-phase 'assembly
                     (lambda () (map make-observed-grammar roles))
                     observed-grammar-batch-valid?)))
    (else (error "expected no arguments, role, or assembly" args))))
