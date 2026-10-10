#!/usr/bin/env gxi
;;; Closed native conformance and source gxtest have distinct artifact owners.
(import (only-in :std/build-script defbuild-script)
        (only-in :std/make make make-clean)
        (only-in :std/source this-source-file)
        (only-in :gerbil/compiler compile-module)
        (only-in :gerbil/expander import-module module-context-export
                 module-export-name module-export-phi))
(load "scripts/test-plan.ss")
(def conformance-modules
  '("t/fixtures/fixture-release"
    "t/fixtures/tla-sany-differential/worker-control"
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
    "t/fixtures/lr1-construction"
    "t/resolved-grammar-test"
    "t/lr-conflict-origins-test"
    "t/language-artifact-test"
    "t/language-entry-boundary-test"
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
(def (shared-test-module? name)
  (or (string-suffix? "-test" name)
      (member name '("t/fixtures/language-entries" "t/conformance-main"
                     "t/benchmarks/versioned-languages/all-languages"))))
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
        optimize: #f generate-ssxi: #t static: #t keep-scm: #t
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
  ;; Helpers serve both the closed executable and source-mode generators.
  ;; Publish complete loadable products; the link owner also consumes static SCM.
  (for-each
   (lambda (module)
     (displayln "STATIC-HELPER-SCHEME " module) (force-output)
     (compile-module (string-append module ".ss")
       [output-dir: (path-expand "lib" (getenv "GERBIL_PATH"))
        optimize: #t generate-ssxi: #t static: #t keep-scm: #t
        invoke-gsc: #t
        gsc-options: ["-verbose" "-cc-options" "-v -Q -ftrack-macro-expansion=0"] verbose: #f]))
   (filter (lambda (name) (not (shared-test-module? name))) conformance-modules))
  ;; Test bodies remain source-only here; their native objects belong to the link owner.
  (compile-static-tests!))
(def (prepare-gxtest!)
  ;; gxtest owns source test evaluation. The closed executable already owns
  ;; native conformance bodies; production parser modules remain native.
  ;; Remove only source-only test interfaces, using std/make's own output map,
  ;; so gxtest imports their Scheme sources rather than an incomplete product.
  (let (modules (filter shared-test-module? conformance-modules))
    (make-clean
     (append (compiled-spec modules)
             (map (lambda (module) `(copy: ,(string-append module ".scm"))) modules))
     libdir: (path-expand "lib" (getenv "GERBIL_PATH"))
     srcdir: (path-directory (this-source-file)))))
;; Developer multicall build retains the standard native shared-module owner.
(defbuild-script (compiled-spec conformance-modules) optimize: #t)
