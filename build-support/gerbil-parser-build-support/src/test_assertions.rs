//! Shared assertions that require no ASP Rust dependency in the caller.

/// Check the lossless-source invariant for any generated Rowan language.
#[macro_export]
macro_rules! assert_rowan_lossless {
    ($parsed:expr, $source:expr) => {
        assert_eq!($parsed.syntax().to_string(), $source)
    };
}
