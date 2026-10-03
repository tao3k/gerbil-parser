;;; -*- Gerbil -*-
;;; Native clan/testing owner for colocated versioned language-pack suites.

(import :std/test
        "../languages/arithmetic/v1/parser-test"
        "../languages/hcl/v2-24/parser-test"
        "../languages/gql/iso-39075-2024/parser-test"
        "../languages/cypher/opencypher-2024-1/parser-test"
        "../languages/tla-plus/core-test"
        "../languages/tla-plus/layout-test")

;; gxtest runs exported leaf suites. Placing existing suite values in a
;; test-suite initializer does not register any TestCase in Gerbil v0.19.
(export arithmetic-v1-parser-test
        hcl-v2-24-parser-test
        gql-iso-39075-2024-parser-test
        opencypher-2024-1-parser-test
        tla-plus-core-parser-test
        tla-plus-layout-parser-test)
