#!/usr/bin/env gxi
;;; Standard currentness and scheduling for reusable Scheme test helpers.
(import (only-in :std/build-script defbuild-script)
        (only-in :std/list/list delete-duplicates/hash)
        (only-in "./src/build-support/preparation-cache.ss" conformance-source-only?)
        (only-in "./src/build-support/compiler-profile.ss" compiler-build-spec)
        (only-in "./src/build-support/library-publication.ss" publication-build-main))
(include "scripts/test-plan.ss")
(export shared-test-build-spec main)

(def (shared-test-build-spec extra?)
  (let (catalog (test-plan-definition "build-language-conformance.ss" 'conformance-modules))
    (unless (and (pair? catalog) (eq? (car catalog) 'quote))
      (error "expected the declared conformance module catalog"))
    (compiler-build-spec
      (append
        (delete-duplicates/hash
          (append (filter (lambda (module) (not (conformance-source-only? module)))
                          (cadr catalog))
                  (if extra?
                    '("t/fixtures/tla-sany-differential/worker-control"
                      "t/abi-datum-support" "t/line-structure-assertions"
                      "t/benchmarks/lr-request-stack/derivation"
                      "t/benchmarks/lr-request-stack/lookahead"
                      "t/scenarios/performance/selective-glr/scenario"
                      "t/benchmarks/lr-request-stack/benchmark"
                      "t/benchmarks/gql/runtime/actors"
                      "t/benchmarks/lr-request-stack/capture-reference"
                      "t/benchmarks/capture-prefix/controls"
                      "t/support/qualification/benchmark-qos"
                      "t/header-delimiter-test" "t/structured-lexical-cases"
                      "t/structured-lexical-test" "t/module-source-cases"
                      "t/module-source-test" "t/fixtures/shared-scanner/bash")
                    '())))
        (if extra?
          (list `(gxc: "t/benchmarks/gql/runtime/compiled-timing"
                   ,@(cond-expand
                       (darwin '("-ld-options" "-Wl,-undefined,dynamic_lookup"))
                       (else '()))))
          '())))))
(defbuild-script (shared-test-build-spec #t))
(set! main (publication-build-main main))
