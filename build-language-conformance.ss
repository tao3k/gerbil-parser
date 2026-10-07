#!/usr/bin/env gxi
;;; Native conformance products have a separate owner from production discovery.
;;; The pinned SDKs use GCC; function/inlining diagnostics report real work.
(import (only-in :std/build-script defbuild-script))
(defbuild-script
  (map (lambda (module)
           `(gxc: ,module "-cc-options" "-v -Q -fopt-info-inline-all"))
       '("languages/arithmetic/parser-test" "languages/bash/parser-test"
         "languages/cypher/parser-test" "languages/fhirpath/parser-test"
         "languages/gql/parser-test" "languages/hcl/parser-test"
         "languages/hl7/parser-test" "languages/tla-plus/parser-test"
         "t/fixtures/language-diagnostics-vocabulary"
         "t/fixtures/source-strategies" "t/fixtures/bash-products"
         "t/language-diagnostics-test" "t/language-surface-test"
         "t/grammar-composition-test" "t/language-artifact-test"
         "t/language-entry-boundary-test" "t/language-topology-test"
         "t/language-loader-test" "t/language-loader-value-test"
         "t/antlr4-source-test" "t/language-pack-metadata-test"
         "t/source-strategy-test" "t/source-grammar-test" "t/command-profile-test" "t/build-strategy-test"
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
         "t/conformance-main")))
