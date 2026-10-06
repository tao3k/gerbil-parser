#!/usr/bin/env gxi
;;; Native conformance products have a separate owner from production discovery.
(import (only-in :std/build-script defbuild-script))
(defbuild-script
  (map (lambda (module)
         `(gxc: ,module "-cc-options"
                ,(cond-expand (darwin "-v") (else "-v -Q"))))
       '("languages/arithmetic/parser-test" "languages/bash/parser-test"
         "languages/cypher/parser-test" "languages/fhirpath/parser-test"
         "languages/gql/parser-test" "languages/hcl/parser-test"
         "languages/hl7/parser-test" "languages/tla-plus/parser-test"
         "t/fixtures/language-diagnostics-vocabulary"
         "t/fixtures/source-strategies"
         "t/language-diagnostics-test" "t/language-surface-test"
         "t/grammar-composition-test" "t/language-artifact-test"
         "t/language-entry-boundary-test" "t/language-topology-test"
         "t/language-loader-test" "t/language-loader-value-test"
         "t/antlr4-source-test" "t/language-pack-metadata-test"
         "t/source-strategy-test" "t/build-strategy-test"
         "t/gql/benchmark-profile-test"
         "t/benchmarks/versioned-languages/all-languages")))
