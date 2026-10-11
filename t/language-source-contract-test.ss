;;; Engine ownership: source-contract admission and UTF-8 scanner checkpoints.
(import (only-in :std/test check test-case test-suite)
        (only-in ./benchmarks/parser-stage-cost/benchmark measure-parser-stages)
        (only-in :gerbil-parser/src/language/source declare-source-language parse-source-language)
        (only-in "fixtures/source-strategies.ss" test-source-strategy)
        (only-in :gerbil-parser/languages/bash/parser parse-bash parse-bash/receipt)
        (only-in :gerbil-parser/languages/gql/parser parse-gql +gql-representative-query+)
        (only-in :gerbil-parser/languages/arithmetic/parser parse-arithmetic)
        (only-in :gerbil-parser/src/runtime/lr-parser current-lr-event-program-enabled?)
        (only-in :gerbil-parser/src/runtime/source-engines LineSourceStrategy.)
        (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/src/runtime/parse-cost current-parser-cost-observer)
        (only-in :gerbil-parser/src/runtime/source-scanner
                 make-source-scanner source-scanner-initial-state
                 source-scanner-step source-scan-state-byte-offset)
        (only-in :gerbil-parser/src/runtime/token token-end token-kind token-start))

(export language-source-contract-test)
(def language-source-contract-test
  (test-suite "language source contracts"
    (test-case "generated drive observation identifies the executed route under either preference"
      (let* ((source "1 + 2") (reference (parse-arithmetic source)))
        (for-each
         (lambda (events?)
           (let (rows '())
             (check (parameterize
                     ((current-lr-event-program-enabled? events?)
                      (current-parser-cost-observer
                       (lambda (stage receipt) (set! rows (cons (cons stage receipt) rows)))))
                      (parse-arithmetic source)) => reference)
             (check (cdr (assq 'completed (cdr (assq 'generated-drive-execution rows)))) => #t)
             (check (assq 'prepared-checkpoint-execution rows) => #f)))
         '(#f #t))))
    (test-case "public GQL observes the selected streaming engine without changing products"
      (for-each
       (lambda (source)
         (let ((reference (parse-gql source)) (rows '()))
           (check
            (parameterize ((current-parser-cost-observer
                            (lambda (stage receipt)
                              (set! rows (cons (cons stage receipt) rows)))))
              (parse-gql source))
            => reference)
           (check (current-parser-cost-observer) => #f)
           (check (pair? (filter (lambda (row) (eq? 'source-recognition (car row))) rows)) => #t)
           (check (pair? (assq 'prepared-checkpoint-execution rows)) => #t)
           (check (pair? (filter (lambda (row) (eq? 'artifact-materialization (car row))) rows)) => #t)))
       (list +gql-representative-query+ "MATCH (" "")))
    (test-case "stage diagnostics publish P50 and P95 for complete requests"
      (let (rows (parameterize ((current-output-port (open-output-string)))
                    (measure-parser-stages parse-bash "echo hi\n" 20)))
        (check (pair? rows) => #t)
        (for-each
         (lambda (row)
           (let ((cpu50 (cdr (assq 'cpu-p50-ms row)))
                 (cpu95 (cdr (assq 'cpu-p95-ms row)))
                 (alloc50 (cdr (assq 'allocation-p50-bytes row)))
                 (alloc95 (cdr (assq 'allocation-p95-bytes row))))
             (check (cdr (assq 'samples row)) => 20)
             (check (and (number? cpu50) (number? cpu95) (>= cpu95 cpu50)) => #t)
             (check (if (zero? (cdr (assq 'allocation-samples row)))
                      (and (not alloc50) (not alloc95))
                      (and (number? alloc50) (number? alloc95) (>= alloc95 alloc50)))
                    => #t))) rows)))
    (test-case "source language rejects an artifact with another digest"
      (let (other
            (declare-source-language
             "bash" "5.3" "different-contract"
             (test-source-strategy (lambda (_) #f) (lambda (_) '())
              (lambda (source _scanner _digest) (parse-bash source)))))
        (check
         (with-catch
          (lambda (_condition) #t)
          (lambda ()
            (parse-source-language other "echo hi\n")
            #f))
         => #t)))
    (test-case "public source observations preserve admitted products and receipts"
      (def (observe parse source (scan-completed? #t))
        (let ((rows '()) (expected (call-with-values (lambda () (parse source)) list)))
          (let (actual
                (parameterize ((current-parser-cost-observer
                                (lambda (name receipt)
                                  (set! rows (cons (cons name receipt) rows)))))
                  (call-with-values (lambda () (parse source)) list)))
            (check actual => expected))
          (check (current-parser-cost-observer) => #f)
          (for-each
           (lambda (stage)
             (check (length (filter (lambda (row) (eq? stage (car row))) rows)) => 1))
           '(source-prepare source-scan artifact-validation))
          (for-each
           (lambda (row)
             (check (cdr (assq 'completed (cdr row)))
                    => (if (eq? (car row) 'source-scan) scan-completed? #t))) rows)))
      (for-each
       (lambda (source)
         (let (completed? (not (string=? source "echo \"")))
           (observe parse-bash source completed?)
           (observe parse-bash/receipt source completed?)))
       '("echo α\n" "echo \"" ""))
      (let (descriptor
            (declare-source-language "lines" "test" "stage-observation"
              (.o (:: self LineSourceStrategy.) root-kind: 'LineFile token-kind: 'Line
                  required-prefix: "M")))
        (def (parse-lines source) (parse-source-language descriptor source))
        (observe parse-lines "Mα\n")
        (observe parse-lines "rejected\n")))
    (test-case "scanner checkpoints retain byte offsets"
      (let* ((scanner
              (make-source-scanner
               "αx" #f
               (lambda (_source offset context _mode)
                 (if (= offset 2)
                   (values #f offset context)
                   (values 'character (fx+ offset 1) context)))))
             (initial (source-scanner-initial-state scanner)))
        (let-values (((first after-first)
                      (source-scanner-step scanner initial 'test)))
          (check (token-start first) => 0)
          (check (token-end first) => 2)
          (check (source-scan-state-byte-offset initial) => 0)
          (check (source-scan-state-byte-offset after-first) => 2)
          (let-values (((second after-second)
                        (source-scanner-step scanner after-first 'test)))
            (check (token-kind second) => 'character)
            (check (token-start second) => 2)
            (check (token-end second) => 3)
            (check (source-scan-state-byte-offset after-second) => 3)))))
))
