//! Shared build and development support for the `gerbil-parser` workspace.
#![forbid(unsafe_code)]

mod policy;
mod rowan_scenarios;

pub use asp_rust::{AspRustConfig, AspRustWorkspacePolicy, default_asp_rust_config};
pub use policy::workspace_policy;
pub use rowan_scenarios::{
    ROWAN_ENGINE_DETERMINISTIC_HOT_PATH_SCENARIO_ID, ROWAN_ENGINE_EVENT_TREE_HOT_PATH_SCENARIO_ID,
    rowan_engine_scenario_package,
};

pub use asp_rust_build_support::{
    AspRustScenario, AspRustScenarioMeasurement, AspRustScenarioObservation,
    AspRustScenarioPackage, asp_rust_scenario, asp_rust_scenario_package,
    measure_asp_rust_scenario, render_asp_rust_scenario_benchmark_toml,
};

#[doc(hidden)]
pub use asp_rust;
