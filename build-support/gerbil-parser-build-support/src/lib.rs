//! Shared dependency-free test macros for the `gerbil-parser` workspace.
#![forbid(unsafe_code)]

mod policy;
mod runtime_scenarios;
mod test_assertions;

pub use runtime_scenarios::{
    RUNTIME_ENGINE_DETERMINISTIC_HOT_PATH_SCENARIO_ID,
    RUNTIME_ENGINE_EVENT_TREE_HOT_PATH_SCENARIO_ID,
};
