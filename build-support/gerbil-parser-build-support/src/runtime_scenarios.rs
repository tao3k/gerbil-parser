//! Rust parser-engine performance red-zone Scenarios.

/// Stable identity for the deterministic engine hot-path Scenario.
pub const RUNTIME_ENGINE_DETERMINISTIC_HOT_PATH_SCENARIO_ID: &str =
    "runtime-engine-deterministic-hot-path-v1";

/// Stable identity for Scheme-AOT structural event emission.
pub const RUNTIME_ENGINE_EVENT_TREE_HOT_PATH_SCENARIO_ID: &str =
    "runtime-engine-event-tree-hot-path-v1";

/// Expand the generated-table Rust red zone in a dev-only test target.
#[macro_export]
macro_rules! runtime_engine_scenario_package {
  () => {
    ::asp_rust_build_support::asp_rust_scenario_package! {
        package: "gerbil-parser-build-support",
        scenarios: [
            ::asp_rust_build_support::asp_rust_scenario! {
                name: $crate::RUNTIME_ENGINE_DETERMINISTIC_HOT_PATH_SCENARIO_ID,
                package: "gerbil-parser-build-support",
                description: "The generic engine keeps generated-table lexing, LR execution, and lossless indexed CST construction inside the hot-path budget",
                fixture_root: "rust/gerbil-parser-runtime-scenarios/tests/performance/scenarios/runtime_engine_deterministic_hot_path_v1",
                tags: ["performance", "red-zone", "runtime", "engine", "aot", "deterministic-lr"],
                commands: [
                    {
                        label: "focused",
                        argv: ["cargo", "test", "--release", "-p", "gerbil-parser-build-support", "--test", "performance_test", "runtime_parse::runtime_engine_deterministic_hot_path_v1", "--", "--ignored", "--exact", "--nocapture"]
                    }
                ],
                benchmark: {
                    harness: "libtest",
                    test: "runtime_parse::runtime_engine_deterministic_hot_path_v1",
                    snapshot: "runtime_engine_deterministic_hot_path_v1",
                    target_total: "5ms",
                    max_total: "10ms",
                    regression_budget: "2ms",
                    memory_budget_bytes: 16_777_216,
                    target_rationale: "Generated-table deterministic execution is the steady-state floor shared by every downstream DSL grammar product.",
                    warmup_iterations: 5,
                    measure_iterations: 31,
                    metrics: [
                        { name: "parse_count", unit: "count", kind: Exact, target: 256 },
                        { name: "runtime_node_count", unit: "count", kind: Minimum, target: 1 },
                        { name: "provider_process_count", unit: "count", kind: Exact, target: 0 }
                    ]
                }
            },
            ::asp_rust_build_support::asp_rust_scenario! {
                name: $crate::RUNTIME_ENGINE_EVENT_TREE_HOT_PATH_SCENARIO_ID,
                package: "gerbil-parser-build-support",
                description: "The generic syntax event sink builds deeply nested lossless CSTs without language-specific parser policy",
                fixture_root: "rust/gerbil-parser-runtime-scenarios/tests/performance/scenarios/runtime_engine_event_tree_hot_path_v1",
                tags: ["performance", "red-zone", "runtime", "engine", "aot", "event-tree"],
                commands: [
                    {
                        label: "focused",
                        argv: ["cargo", "test", "--release", "-p", "gerbil-parser-build-support", "--test", "performance_test", "runtime_parse::runtime_engine_event_tree_hot_path_v1", "--", "--ignored", "--exact", "--nocapture"]
                    }
                ],
                benchmark: {
                    harness: "libtest",
                    test: "runtime_parse::runtime_engine_event_tree_hot_path_v1",
                    snapshot: "runtime_engine_event_tree_hot_path_v1",
                    target_total: "5ms",
                    max_total: "10ms",
                    regression_budget: "2ms",
                    memory_budget_bytes: 16_777_216,
                    target_rationale: "Structural event emission must stay bounded before downstream Org parser logic is admitted.",
                    warmup_iterations: 5,
                    measure_iterations: 31,
                    metrics: [
                        { name: "parse_count", unit: "count", kind: Exact, target: 128 },
                        { name: "runtime_node_count", unit: "count", kind: Minimum, target: 16512 },
                        { name: "provider_process_count", unit: "count", kind: Exact, target: 0 }
                    ]
                }
            }
        ]
    }
  };
}
