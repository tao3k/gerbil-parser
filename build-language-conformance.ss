#!/usr/bin/env gxi
;;; Closed conformance and loadable gxtest modules are separate native products.
(import (only-in :std/build-script defbuild-script)
        (only-in :std/make make)
        (only-in :std/source this-source-file)
        (only-in :gerbil/compiler compile-module execute-pending-compile-jobs!))
(def conformance-modules
  '("languages/arithmetic/parser-test"
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
    "t/grammar-composition-test"
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
    "t/generate-native-language-alignment"
    "t/conformance-main"))
(def (shared-test-module? name)
  (or (string-suffix? "-test" name)
      (member name '("t/generate-native-language-alignment" "t/conformance-main"
                     "t/benchmarks/versioned-languages/all-languages"))))
(def (native-spec modules)
  (map (lambda (module)
         `(gxc: ,module "-cc-options" "-v -Q -fopt-info-inline-optimized")) modules))
(def (compile-test-modules! native?)
  ;; Match std/make source admission before compiling mutually referring interfaces.
  (add-load-path! (path-directory (this-source-file)))
  (for-each
   (lambda (module)
     (when (and native?
                (member module '("t/generate-native-language-alignment" "t/conformance-main")))
       (execute-pending-compile-jobs!))
     (displayln (if native? "SHARED-TEST-COMPILE " "STATIC-TEST-SCHEME ") module)
     (force-output)
     (compile-module (string-append module ".ss")
       [output-dir: (path-expand "lib" (getenv "GERBIL_PATH"))
        optimize: #f generate-ssxi: #t static: #t keep-scm: #t
        invoke-gsc: native? parallel: native? verbose: #t
        gsc-options: ["-cc-options" "-v -Q -fopt-info-inline-optimized"]]))
   (filter shared-test-module? conformance-modules))
  (when native? (execute-pending-compile-jobs!)))
(def (compile-static-conformance!)
  ;; Source-expansion research children need these loadable helper products.
  (make (native-spec (filter (lambda (name) (not (shared-test-module? name))) conformance-modules))
    srcdir: (path-directory (this-source-file)) optimize: #f)
  ;; The closed executable owns their only required C/object compilation here.
  (compile-test-modules! #f))
(def (compile-shared-tests!)
  ;; Later gxtest source owners import test fixtures through loadable modules.
  (compile-test-modules! #t))
;; Developer multicall build retains the standard native shared-module owner.
(defbuild-script (native-spec conformance-modules) optimize: #f)
