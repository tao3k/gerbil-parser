#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Acceptance owner for POO Flow source-authoring, memory, and timing evidence.
;;; Runtime observation remains an opt-in POO policy carried by LanguageGrammar.

(import :std/test
        (only-in :clan/poo/object .ref)
        (only-in :std/misc/ports read-all-as-string)
        (only-in :std/srfi/13 string-contains)
        :asp-gerbil-scheme/src/benchmark/framework
        :poo-flow/src/module-system/observability/interface
        :gerbil-parser/src/modules/parser/interface
        "./scenarios/observability/opencypher-grammar-phases/scenario")

(def benchmark-path "t/benchmarks/poo-grammar-objects/benchmark.ss")

(def parser-poo-source-files
  '("src/modules/parser/types.ss"
    "src/modules/parser/objects.ss"
    "src/modules/parser/funcs.ss"
    "src/modules/parser/syntax.ss"
    "src/modules/parser/interface.ss"))

(def (make-observed-role)
  (make-grammar-role
   'observability-role '() '() '() '() '() '() '() '() '()))

(def (make-observed-grammar)
  (let (role (make-observed-role))
    (make-grammar 'observability-grammar '() (list role))))

(def (make-observed-grammar-batch)
  (let loop ((remaining 50) (grammars '()))
    (if (zero? remaining)
      grammars
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

(def poo-observability-tests
  (test-suite "POO Flow observability for parser grammar objects"
    (test-case "runtime observation imports only the POO debug boundary"
      (let (source
            (call-with-input-file
             "src/runtime/observability.ss" read-all-as-string))
        (check
         (and (string-contains
               source
               ":poo-flow/src/module-system/observability/debug")
              (not (string-contains
                    source
                    ":poo-flow/src/module-system/observability/interface")))
         => #t)))
    (test-case "parser POO sources pass the reader-native authoring gate"
      (for-each
       (lambda (path)
         (check
          (list path
                (map poo-flow-authoring-observation-sexp
                     (poo-flow-authoring-inline-prototype-file-observations
                      'gerbil-parser path)))
          => (list path '())))
       parser-poo-source-files))
    (test-case "checked object construction emits a bounded POO memory receipt"
      (let (policy
            (poo-flow-debug-memory-policy
             'gerbil-parser
             heap-limit-bytes: 4294967296
             live-growth-limit-bytes: 67108864
             collect-before-sample?: #t))
        (call-with-values
          (lambda ()
            (call-with-poo-flow-debug-memory-span
             policy 'grammar-object-construction
             make-observed-grammar-batch))
          (lambda (grammars receipt)
            (check (observed-grammar-batch-valid? grammars) => #t)
            (check (.ref receipt 'accepted?) => #t)
            (check (.ref receipt 'reason) => 'within-budget)
            (check (car (poo-flow-debug-memory-receipt-sexp receipt))
                   => 'debug-memory-observation)))))
    (test-case "checked object construction satisfies the ASP benchmark gate"
      (check (benchmark-contract-valid? benchmark-path) => #t)
      (check (observed-grammar-batch-valid?
              (make-observed-grammar-batch))
             => #t)
      (##gc)
      (let (receipt
            (benchmark-contract-run benchmark-path
                                    make-observed-grammar-batch))
        (write receipt)
        (newline)
        (check (benchmark-contract-receipt-pass? receipt) => #t)))
    (test-case "LanguageGrammar atomically configures POO Flow phase observation"
      (let (receipt (opencypher-grammar-observability-scenario))
        (write receipt)
        (newline)
        (check (opencypher-grammar-observability-scenario-pass? receipt)
               => #t)))))

(run-tests! poo-observability-tests)
