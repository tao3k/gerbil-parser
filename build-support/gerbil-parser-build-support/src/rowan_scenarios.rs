//! Rust/Rowan parser-engine performance red-zone Scenarios.

use crate::{AspRustScenarioPackage, asp_rust_scenario, asp_rust_scenario_package};

/// Stable identity for the deterministic engine hot-path Scenario.
pub const ROWAN_ENGINE_DETERMINISTIC_HOT_PATH_SCENARIO_ID: &str =
    "rowan-engine-deterministic-hot-path-v1";

/// Stable identity for the selective-GLR engine scale Scenario.
pub const ROWAN_ENGINE_SELECTIVE_GLR_SCALE_SCENARIO_ID: &str =
    "rowan-engine-selective-glr-scale-v1";

/// Package the generated-table Rust/Rowan latency red zone as ASP Rust Scenarios.
#[must_use]
pub fn rowan_engine_scenario_package() -> AspRustScenarioPackage {
    asp_rust_scenario_package! {
        package: "gerbil-parser-rowan",
        scenarios: [
            asp_rust_scenario! {
                name: ROWAN_ENGINE_DETERMINISTIC_HOT_PATH_SCENARIO_ID,
                package: "gerbil-parser-rowan",
                description: "The generic engine keeps generated-table lexing, LR execution, and lossless Rowan CST construction inside the hot-path budget",
                fixture_root: "rust/gerbil-parser-rowan/tests/performance/scenarios/rowan_engine_deterministic_hot_path_v1",
                tags: ["performance", "red-zone", "rowan", "engine", "aot", "deterministic-lr"],
                commands: [
                    {
                        label: "focused",
                        argv: ["cargo", "test", "--release", "-p", "gerbil-parser-rowan", "--test", "performance_test", "rowan_parse::rowan_engine_deterministic_hot_path_v1", "--", "--ignored", "--exact", "--nocapture"]
                    }
                ],
                benchmark: {
                    harness: "libtest",
                    test: "rowan_parse::rowan_engine_deterministic_hot_path_v1",
                    snapshot: "rowan_engine_deterministic_hot_path_v1",
                    target_total: "5ms",
                    max_total: "10ms",
                    regression_budget: "2ms",
                    memory_budget_bytes: 16_777_216,
                    target_rationale: "Generated-table deterministic execution is the steady-state floor shared by every downstream DSL grammar product.",
                    warmup_iterations: 5,
                    measure_iterations: 31,
                    metrics: [
                        { name: "parse_count", unit: "count", kind: Exact, target: 256 },
                        { name: "rowan_node_count", unit: "count", kind: Minimum, target: 1 },
                        { name: "provider_process_count", unit: "count", kind: Exact, target: 0 }
                    ]
                }
            },
            asp_rust_scenario! {
                name: ROWAN_ENGINE_SELECTIVE_GLR_SCALE_SCENARIO_ID,
                package: "gerbil-parser-rowan",
                description: "The generic engine keeps generated selective-GLR tables and lossless Rowan CST construction bounded under a production-scale grammar workload",
                fixture_root: "rust/gerbil-parser-rowan/tests/performance/scenarios/rowan_engine_selective_glr_scale_v1",
                tags: ["performance", "red-zone", "rowan", "engine", "aot", "selective-glr", "scale"],
                commands: [
                    {
                        label: "focused",
                        argv: ["cargo", "test", "--release", "-p", "gerbil-parser-rowan", "--test", "performance_test", "rowan_parse::rowan_engine_selective_glr_scale_v1", "--", "--ignored", "--exact", "--nocapture"]
                    }
                ],
                benchmark: {
                    harness: "libtest",
                    test: "rowan_parse::rowan_engine_selective_glr_scale_v1",
                    snapshot: "rowan_engine_selective_glr_scale_v1",
                    target_total: "5ms",
                    max_total: "15ms",
                    regression_budget: "5ms",
                    memory_budget_bytes: 67_108_864,
                    target_rationale: "The AOT engine must keep production-scale selective-GLR work bounded independent of downstream semantic lowering.",
                    warmup_iterations: 5,
                    measure_iterations: 31,
                    metrics: [
                        { name: "source_count", unit: "count", kind: Exact, target: 14 },
                        { name: "rowan_node_count", unit: "count", kind: Minimum, target: 1 },
                        { name: "selective_glr_branch_count", unit: "count", kind: Minimum, target: 1 },
                        { name: "provider_process_count", unit: "count", kind: Exact, target: 0 }
                    ]
                }
            }
        ]
    }
}
