//! Shared build and development support for the `gerbil-parser` workspace.
#![forbid(unsafe_code)]

mod policy;

pub use asp_rust::{AspRustConfig, AspRustWorkspacePolicy, default_asp_rust_config};
pub use policy::workspace_policy;

#[doc(hidden)]
pub use asp_rust;
