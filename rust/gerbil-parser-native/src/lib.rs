//! Rust ownership for an already initialized, owner-thread Gerbil language pack.
//!
//! The embedding host initializes the VM and keeps the library loaded until all
//! sessions are dropped. This crate owns handles and C results, not VM startup.
mod abi;
mod language;
mod session;

pub use abi::{LanguageApi, RawResult};
pub use language::{NativeLanguage, NativeParsed, NativePayload};
pub use session::{NativeError, NativeSession};
