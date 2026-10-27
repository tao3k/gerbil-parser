#!/usr/bin/env gxi
;;; Standard currentness and scheduling for reusable Scheme test helpers.
(import (only-in :std/build-script defbuild-script)
        (only-in :std/source this-source-file)
        (only-in "./src/build-support/compiler-profile.ss" compiler-build-spec))
(import (only-in :clan/poo/object .get .call)
        (only-in "./init.ss" gerbil-parser.init.exports))
(export shared-test-build-spec main)

;; Init owns discovery, native import closure and source currentness. This is
;; exactly the same native Suite roster as gerbil test t/... languages/... .
(def (test-support-source)
  (let* ((tests (.get gerbil-parser.init.exports tests))
         (files (append (.get (.call tests .for-directory "t") spec)
                        (.get (.call tests .for-directory "languages") spec))))
    (.call tests .for-files files)))

(def (shared-test-build-spec extra?)
  (append
    (compiler-build-spec ((.get (test-support-source) std-make-spec)))
    ;; Standalone benchmark products are native build roots, not a parallel
    ;; Suite roster. Imported fixture helpers are discovered by Core above.
    (if extra?
      (compiler-build-spec
        '("t/abi-datum-support" "t/line-structure-assertions"
          "t/benchmarks/lr-request-stack/derivation"
          "t/benchmarks/lr-request-stack/lookahead"
          "t/scenarios/performance/selective-glr/scenario"
          "t/benchmarks/lr-request-stack/benchmark"
          "t/benchmarks/gql/runtime/actors"
          "t/benchmarks/lr-request-stack/capture-reference"
          "t/benchmarks/capture-prefix/controls"
          "t/support/qualification/benchmark-qos"))
      '())
    (if extra?
      (list `(gxc: "t/benchmarks/gql/runtime/compiled-timing"
               ,@(cond-expand
                   (darwin '("-ld-options" "-Wl,-undefined,dynamic_lookup"))
                   (else '()))))
      '())))
(defbuild-script (shared-test-build-spec #t))
