#!/usr/bin/env gxi
;;; One standard build owner for the engine evidence workload closure.
(import :std/build-script)
(defbuild-script
  '("../../languages/arithmetic/parser-test"
    "../../languages/hcl/parser-test"
    "parser-stage-cost/benchmark"
    "../scenarios/performance/selective-glr/scenario"
    "lr-request-stack/derivation"
    "lr-request-stack/lookahead"
    "gql/runtime/execution-counts"
    "gql/runtime/reduction-counts"
    "gql/runtime/matched-stages"
    "lr-request-stack/benchmark"
    "parser-evidence"))
