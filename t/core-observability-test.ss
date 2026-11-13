#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-
;;; Acceptance owner for parser Core memory evidence.
;;; Runtime observation remains an opt-in POO policy carried by LanguageGrammar.

(import :std/test
        (only-in :clan/poo/object .ref)
        (only-in :std/misc/ports read-all-as-string)
        :tao3k/yuanxu/observability/debug
        (only-in "./benchmarks/poo-grammar-objects/construction.ss"
                 make-observed-grammar-batch observed-grammar-batch-valid?))

(deflanguage-parser-tests core-observability-tests "Core observability for parser grammar objects"
  (engine)
    (property "runtime observation imports only the POO debug boundary"
      (bindings (source
            (call-with-input-file
             "src/runtime/observability.ss" read-all-as-string)))
      (equal
         (and (string-contains
               source
               ":tao3k/yuanxu/observability/debug")
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
                    'debug-memory-observation)))))


;; gxtest discovers only exported names ending in -test.
(def core-observability-test core-observability-tests)
(export core-observability-test)

;; Resource admission owns its measurement interval in the shared runtime.
