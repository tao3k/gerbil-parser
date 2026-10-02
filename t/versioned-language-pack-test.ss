;;; -*- Gerbil -*-
;;; Native clan/testing owner for colocated versioned language-pack suites.

(import :std/test
        "../languages/arithmetic/v1/parser-test"
        "../languages/hcl/v2-24/parser-test"
        "../languages/gql/iso-39075-2024/parser-test"
        "../languages/cypher/opencypher-2024-1/parser-test"
        "../languages/tla-plus/core-test"
        "../languages/tla-plus/layout-test")

(export versioned-language-pack-test)

(def versioned-language-pack-test
  (test-suite "versioned language-pack acceptance"
    arithmetic-v1-parser-test
    hcl-v2-24-parser-test
    gql-iso-39075-2024-parser-test
    opencypher-2024-1-parser-test
    tla-plus-core-parser-test
    tla-plus-layout-parser-test))
