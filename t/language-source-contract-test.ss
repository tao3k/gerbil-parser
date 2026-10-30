(import (only-in :clan/poo/type OrFalse Symbol))
(import :gerbil-parser/language-test-support)
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


(deflanguage-parser-tests language-source-contract-test "language source contracts"
  (engine)
    (scenario "generated drive observation identifies the executed route under either preference"
      (sequential ((source "1 + 2") (reference (parse-arithmetic source)))
        (each (events?) ('(#f #t))
           (scope ((rows '()))
             (equal (parameterize
                     ((current-lr-event-program-enabled? events?)
                      (current-parser-cost-observer
                       (lambda (stage receipt) (set! rows (cons (cons stage receipt) rows)))))
                      (parse-arithmetic source))  reference)
             (equal (cdr (assq 'completed (cdr (assq 'generated-drive-execution rows))))  #t)
             (equal (assq 'prepared-checkpoint-execution rows)  #f)))))
    (scenario "public GQL observes the selected streaming engine without changing products"
      (each (source) ((list +gql-representative-query+ "MATCH (" ""))
         (scope ((reference (parse-gql source)) (rows '()))
           (equal
            (parameterize ((current-parser-cost-observer
                            (lambda (stage receipt)
                              (set! rows (cons (cons stage receipt) rows)))))
              (parse-gql source))
             reference)
           (equal (current-parser-cost-observer)  #f)
           (equal (pair? (filter (lambda (row) (eq? 'source-recognition (car row))) rows))  #t)
           (equal (pair? (assq 'prepared-checkpoint-execution rows))  #t)
           (equal (pair? (filter (lambda (row) (eq? 'artifact-materialization (car row))) rows))  #t))))
    (scenario "stage diagnostics publish P50 and P95 for complete requests"
      (scope ((rows (parameterize ((current-output-port (open-output-string)))
                    (measure-parser-stages parse-bash "echo hi\n" 20))))
        (equal (pair? rows)  #t)
        (each (row) (rows)
           (scope ((cpu50 (cdr (assq 'cpu-p50-ms row)))
                 (cpu95 (cdr (assq 'cpu-p95-ms row)))
                 (alloc50 (cdr (assq 'allocation-p50-bytes row)))
                 (alloc95 (cdr (assq 'allocation-p95-bytes row))))
             (equal (cdr (assq 'samples row))  20)
             (equal (and (number? cpu50) (number? cpu95) (>= cpu95 cpu50))  #t)
             (equal (if (zero? (cdr (assq 'allocation-samples row)))
                      (and (not alloc50) (not alloc95))
                      (and (number? alloc50) (number? alloc95) (>= alloc95 alloc50)))
                     #t)))))
    (property "source language rejects an artifact with another digest"
      (bindings (other
            (declare-source-language
             "bash" "5.3" "different-contract"
             (test-source-strategy (lambda (_) #f) (lambda (_) '())
              (lambda (source _scanner _digest) (parse-bash source))))))
      (equal
         (with-catch
          (lambda (_condition) #t)
          (lambda ()
            (parse-source-language other "echo hi\n")
            #f))
          #t))
    (scenario "public source observations preserve admitted products and receipts"
(locals ((observe (parser-check ( parse source (scan-completed? #t))
        (scope ((rows '()) (expected (call-with-values (lambda () (parse source)) list)))
          (scope ((actual
                (parameterize ((current-parser-cost-observer
                                (lambda (name receipt)
                                  (set! rows (cons (cons name receipt) rows)))))
                  (call-with-values (lambda () (parse source)) list))))
            (equal actual  expected))
          (equal (current-parser-cost-observer)  #f)
          (each (stage) ('(source-prepare source-scan artifact-validation))
             (equal (length (filter (lambda (row) (eq? stage (car row))) rows))  1))
          (each (row) (rows)
             (equal (cdr (assq 'completed (cdr row)))
                     (if (eq? (car row) 'source-scan) scan-completed? #t)))))))

      (each (source) ('("echo α\n" "echo \"" ""))
         (scope ((completed? (not (string=? source "echo \""))))
           (verify observe parse-bash source completed?)
           (verify observe parse-bash/receipt source completed?)))
      (scope ((descriptor
            (declare-source-language "lines" "test" "stage-observation"
              (.o (:: self LineSourceStrategy.) root-kind: 'LineFile token-kind: 'Line
                  required-prefix: "M"))))
(locals ((parse-lines (lambda ( source) (parse-source-language descriptor source))))

        (verify observe parse-lines "Mα\n")
        (verify observe parse-lines "rejected\n")))))
    (scenario "scanner checkpoints retain byte offsets"
      (sequential ((scanner
              (make-source-scanner (OrFalse Symbol) Symbol
               "αx" #f
               (lambda (_source offset context _mode)
                 (if (= offset 2)
                   (values #f offset context)
                   (values 'character (fx+ offset 1) context)))))
             (initial (source-scanner-initial-state scanner)))
        (scope-values (((first after-first)
                      (source-scanner-step scanner initial 'test)))
          (equal (token-start first)  0)
          (equal (token-end first)  2)
          (equal (source-scan-state-byte-offset initial)  0)
          (equal (source-scan-state-byte-offset after-first)  2)
          (scope-values (((second after-second)
                        (source-scanner-step scanner after-first 'test)))
            (equal (token-kind second)  'character)
            (equal (token-start second)  2)
            (equal (token-end second)  3)
            (equal (source-scan-state-byte-offset after-second)  3)))))
)
