#!/usr/bin/env gxi
;;; Closed native conformance and source gxtest have distinct artifact owners.
(import (only-in "./src/build-support/compiler-profile.ss" compiler-build-spec initialize-compiler-build!)
        (only-in "./build-test-support.ss" shared-test-build-spec))
(import (only-in :std/build-script defbuild-script)
        (only-in :std/make make make-clean)
        (only-in :std/source this-source-file)
        (only-in :gerbil/compiler compile-module)
        (only-in :gerbil/expander import-module module-context-export
                 module-export-name module-export-phi))
(include "scripts/test-plan.ss")
(def conformance-modules
  '("t/fixtures/fixture-release"
    "t/fixtures/grammar-composition-models"
    "t/fixtures/language-pack-research/package-expression-runtime"
    "t/fixtures/language-pack-research/composition-history"
    "t/fixtures/language-pack-research/list-runtime"
    "t/fixtures/language-pack-research/provenance-language"
    "languages/arithmetic/parser-test"
    "languages/bash/parser-test"
    "languages/cypher/parser-test"
    "languages/fhirpath/parser-test"
    "languages/gql/parser-test"
    "languages/hcl/parser-test"
    "languages/hl7/parser-test"
    "languages/tla-plus/parser-test"
    "t/fixtures/language-diagnostics-vocabulary"
    "t/fixtures/source-strategies"
    "t/fixtures/bash-products"
    "t/language-diagnostics-test"
    "t/language-surface-test"
    "t/grammar-composition-contract-test"
    "t/grammar-composition-lowering-test"
    "t/grammar-composition-execution-test"
    "t/fixtures/language-entries"
    "t/fixtures/prediction-witness"
    "t/fixtures/event-walk-source"
    "t/fixtures/lr1-construction"
    "t/resolved-grammar-test"
    "t/nullable-origin-test"
    "t/lr-conflict-origins-test"
    "t/fixtures/layout-columns"
    "t/layout-column-storage-test"
    "t/language-artifact-test"
    "t/benchmarks/parser-stage-cost/benchmark"
    "t/fixtures/lr-action-selection"
    "t/benchmarks/lr-request-stack/lookahead"
    "t/lr-action-preparation-test"
    "t/fixtures/ranked-publication"
    "t/ranked-parser-request-test"
    "t/benchmarks/ranked-literal/benchmark"
    "t/benchmarks/ranked-literal/parser-cpu"
    "t/benchmarks/ranked-literal/lexical-cpu"
    "t/benchmarks/layout-columns/benchmark"
    "t/artifact-publication-cost-test"
    "t/fixtures/artifact-window"
    "t/artifact-window-publication-test"
    "t/language-entry-boundary-test"
    "t/native-function-boundary-test"
    "t/language-topology-test"
    "t/language-loader-test"
    "t/language-loader-value-test"
    "t/antlr4-source-test"
    "t/language-pack-metadata-test"
    "t/source-strategy-test"
    "t/source-grammar-test"
    "t/command-profile-test"
    "t/build-strategy-test"
    "t/gql/benchmark-profile-test"
    "t/benchmarks/versioned-languages/all-languages"
    "t/benchmarks/gql/runtime/reduction-counts"
    "t/benchmarks/gql/runtime/execution-counts"
    "t/benchmarks/gql/runtime/matched-stages"
    "t/fixtures/language-pack-research/package-expression-grammar"
    "t/fixtures/language-pack-research/package-expression-parser"
    "t/fixtures/language-pack-research/package-expression-parser-test"
    "t/fixtures/language-pack-research/list-roles"
    "t/fixtures/language-pack-research/list-origins"
    "t/fixtures/language-pack-research/list-stage"
    "t/fixtures/language-pack-research/list-language"
    "t/fixtures/language-pack-research/list-parser"
    "t/fixtures/language-pack-research/list-parser-test"
    "t/fixtures/runtime-record-assignments/languages/records/grammar"
    "t/fixtures/runtime-record-assignments/languages/records/fixtures"
    "t/fixtures/runtime-record-assignments/languages/records/parser"
    "t/fixtures/runtime-record-assignments/languages/records/parser-test"
    "t/test-style-contract-test"
    "t/conformance-main"))
(def shared-test-module? conformance-source-only?)
(def (compiled-spec modules)
  (map (lambda (module)
         `(gxc: ,module "-cc-options" "-v -Q")) modules))
(def (compile-static-tests!)
  ;; Keep source expansion and executable publication in one compiler context.
  (add-load-path! (path-directory (this-source-file)))
  (for-each
   (lambda (module)
     (displayln "STATIC-TEST-SCHEME " module) (force-output)
     (compile-module (string-append module ".ss")
       [output-dir: (path-expand "lib" (getenv "GERBIL_PATH"))
        ;; Stage test construction with the Gerbil optimizer before native
        ;; lowering to reduce generated host size and compiler work. Keep
        ;; every assertion and the native tail-call compilation policy.
        optimize: #t generate-ssxi: #t static: #t keep-scm: #t
        invoke-gsc: #f verbose: #f])
     (when (string-suffix? "-test" module)
       (displayln "CONFORMANCE-EXPORT-CHECK " module) (force-output)
       ;; Inspect the public expansion context without evaluating test bodies.
       (let (context (import-module (string-append module ".ss") #f #f))
         (validate-test-suite-ownership!
          (conformance-imported-test-names module)
          (conformance-source-suite-names module)
          (map module-export-name
               (filter (lambda (exported)
                         (and (zero? (module-export-phi exported))
                              (string-suffix? "-test" (symbol->string (module-export-name exported)))))
                       (module-context-export context)))))))
   (filter shared-test-module? conformance-modules)))
(def (compile-static-conformance!)
  ;; The shared native graph is scheduled once by std/make. Only catalog
  ;; helpers belong to this declaration; extra source-test helpers
  ;; remain the sibling build-test-support owner's products.
  (initialize-compiler-build!)
  (make (shared-test-build-spec #f)
        srcdir: (path-directory (this-source-file))
        libdir: (path-expand "lib" (gerbil-path))
        optimize: #t)
  ;; Test bodies remain source-only here; their native objects belong to the link owner.
  (compile-static-tests!))
;; Developer multicall build retains the standard native shared-module owner.
(defbuild-script (compiler-build-spec (compiled-spec conformance-modules)) optimize: #t)
