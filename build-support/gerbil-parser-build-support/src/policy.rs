//! ASP Rust policy owned by workspace Build Support.

use crate::{AspRustWorkspacePolicy, default_asp_rust_config};

/// Return the single ASP Rust policy owned by this workspace.
#[must_use]
pub fn workspace_policy() -> AspRustWorkspacePolicy {
    AspRustWorkspacePolicy::new("gerbil-parser", default_asp_rust_config())
}

/// Mount the workspace ASP Rust policy as a Cargo-test-only Dev Gate.
///
/// The workspace starts in warning mode while the existing Rust surface is
/// brought under the policy. Change this one mode to `deny` when that debt is
/// closed.
#[macro_export]
macro_rules! asp_workspace_policy_gate {
    () => {
        $crate::asp_rust::asp_rust_workspace_dev_gate!(
            mode = warn,
            policy = $crate::workspace_policy()
        );
    };
}
