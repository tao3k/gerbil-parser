//! ASP Rust policy owned by workspace Build Support.

use crate::{AspRustWorkspacePolicy, default_asp_rust_config};

/// Return the single ASP Rust policy owned by this workspace.
#[must_use]
pub fn workspace_policy() -> AspRustWorkspacePolicy {
    let mut config = default_asp_rust_config();
    // Generated language tables are immutable compiler output verified against
    // Grammar IR in native CI. Policy governs their generator and the runtime
    // owners instead of treating hundreds of thousands of table rows as
    // handwritten module responsibilities.
    config.ignored_dir_names.insert("generated".to_owned());
    AspRustWorkspacePolicy::new("gerbil-parser", config)
}

/// Mount the workspace ASP Rust policy as a Cargo-test-only Dev Gate.
///
/// Warnings and errors are test-blocking; ASP Rust remains a dev dependency.
#[macro_export]
macro_rules! asp_workspace_policy_gate {
    () => {
        $crate::asp_rust::asp_rust_workspace_dev_gate!(
            mode = deny,
            policy = $crate::workspace_policy()
        );
    };
}
