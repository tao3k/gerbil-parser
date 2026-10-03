//! Shared dependency-free test macros for the `gerbil-parser` workspace.
#![forbid(unsafe_code)]

mod policy;
mod rowan_scenarios;
mod test_assertions;

pub use rowan_scenarios::{
    ROWAN_ENGINE_DETERMINISTIC_HOT_PATH_SCENARIO_ID, ROWAN_ENGINE_EVENT_TREE_HOT_PATH_SCENARIO_ID,
};
