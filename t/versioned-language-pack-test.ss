;;; -*- Gerbil -*-
;;; Native clan/testing owner for colocated versioned language-pack suites.

(import :std/test
        "../languages/arithmetic/parser-test"
        "../languages/hcl/parser-test"
        "../languages/gql/parser-test"
        "../languages/cypher/parser-test"
        "../languages/tla-plus/parser-test")

;; gxtest runs exported leaf suites. Placing existing suite values in a
;; test-suite initializer does not register any TestCase in Gerbil v0.19.
(export arithmetic-parser-test
        hcl-parser-test
        gql-parser-test
        opencypher-parser-test
        tla-plus-core-parser-test
        tla-plus-layout-parser-test
        tla-plus-sany-candidate-parser-test sany-closure-test)
