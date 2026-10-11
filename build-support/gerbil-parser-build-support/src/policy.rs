//! Expand the ASP Rust policy in a test target, never in the support library.

/// Mount the workspace ASP Rust policy as a Cargo-test-only Dev Gate.
#[macro_export]
macro_rules! asp_workspace_policy_gate {
    () => {
        fn workspace_policy() -> ::asp_rust::AspRustWorkspacePolicy {
            let mut config = ::asp_rust::default_asp_rust_config();
            // Generated tables are checked against Grammar IR in native CI.
            config.ignored_dir_names.insert("generated".to_owned());
            ::asp_rust::AspRustWorkspacePolicy::new("gerbil-parser", config)
        }

        ::asp_rust::asp_rust_workspace_dev_gate!(mode = deny, policy = workspace_policy());
    };
}
