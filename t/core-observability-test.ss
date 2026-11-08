#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-
;;; Acceptance owner for parser Core memory and timing evidence.
;;; Runtime observation remains an opt-in POO policy carried by LanguageGrammar.

(import :std/test
        (only-in :clan/poo/object .ref)
        (only-in :std/misc/ports read-all-as-string)
        :asp-gerbil-scheme/src/benchmark/framework
        :yuanxu/observability/debug
        :gerbil-parser/src/declarations
        (only-in :gerbil-parser/t/fixtures/progress report-test-progress!))

(def benchmark-path "t/benchmarks/poo-grammar-objects/benchmark.ss")

(def (make-observed-role)
  (make-grammar-role
   'observability-role '() '() '() '() '() '() '() '() '()))

(def (make-observed-grammar)
  (let (role (make-observed-role))
    (make-grammar 'observability-grammar '() (list role))))

(def (make-observed-grammar-batch)
  (let loop ((remaining 50) (grammars '()))
    (if (zero? remaining)
      (begin (report-test-progress! "OBJECT-BATCH-OK grammars=50") grammars)
      (loop (- remaining 1)
            (cons (make-observed-grammar) grammars)))))

(def (observed-grammar-batch-valid? grammars)
  (and (= (length grammars) 50)
       (andmap
        (lambda (grammar)
          (and (grammar? grammar)
               (= (length (grammar-roles grammar)) 1)
               (grammar-role? (car (grammar-roles grammar)))))
        grammars)))

(deflanguage-parser-tests core-observability-tests "Core observability for parser grammar objects"
  (engine)
    (property "runtime observation imports only the POO debug boundary"
      (bindings (source
            (call-with-input-file
             "src/runtime/observability.ss" read-all-as-string)))
      (equal
         (and (string-contains
               source
               ":yuanxu/observability/debug")
              (not (string-contains
                    source
                    ":poo-flow/src/module-system/observability/interface")))
          #t))
    (scenario "checked object construction emits a bounded POO memory receipt"
      (scope ((policy
            (yuanxu-debug-memory-policy
             'gerbil-parser
             heap-limit-bytes: 4294967296
             live-growth-limit-bytes: 67108864
             collect-before-sample?: #t)))
        (scope-values (((grammars receipt) (call-with-yuanxu-debug-memory-span
             policy 'grammar-object-construction
             make-observed-grammar-batch)))
            (equal (observed-grammar-batch-valid? grammars)  #t)
            (equal (.ref receipt 'accepted?)  #t)
            (equal (.ref receipt 'reason)  'within-budget)
            (equal (car (yuanxu-debug-memory-receipt-sexp receipt))
                    'debug-memory-observation))))
    (scenario "checked object construction satisfies the ASP benchmark gate"
      (equal (benchmark-contract-valid? benchmark-path)  #t)
      (equal (observed-grammar-batch-valid?
              (make-observed-grammar-batch))
              #t)
      (act (##gc))
      (scope ((receipt
            (benchmark-contract-run benchmark-path
                                    make-observed-grammar-batch)))
        (receipt receipt)
        (equal (benchmark-contract-receipt-pass? receipt)  #t))))


;; gxtest discovers only exported names ending in -test.
(def core-observability-test core-observability-tests)
(export core-observability-test)

;; Resource admission owns its measurement interval in the shared runtime.
