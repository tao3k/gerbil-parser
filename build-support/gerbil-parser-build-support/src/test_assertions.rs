//! Shared assertions that require no ASP Rust dependency in the caller.

/// Check the lossless-source invariant for any generated AOT language.
#[macro_export]
macro_rules! assert_syntax_lossless {
    ($parsed:expr, $source:expr) => {
        assert_eq!($parsed.syntax().to_string(), $source)
    };
}
