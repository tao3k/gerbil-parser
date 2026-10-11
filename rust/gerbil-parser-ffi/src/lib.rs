//! Rust ownership for an already initialized, owner-thread Gerbil language pack.
//!
//! Use `NativeRuntime` with a standalone bundle, or `NativeSession` with an
//! embedding host that already initialized the VM. Libraries stay loaded until
//! the runtime/session and all their borrowers are dropped.
mod abi;
mod language;
#[cfg(feature = "standalone")]
mod runtime;
mod session;

#[cfg(feature = "standalone")]
pub use abi::RuntimeApi;
pub use abi::{LanguageApi, RawResult};
pub use language::{NativeLanguage, NativeParsed, NativePayload};
#[cfg(feature = "standalone")]
pub use runtime::NativeRuntime;
pub use session::{NativeError, NativeSession};

pub use gerbil_parser_artifact::{
    NativeArtifactView, NativeCatalog, NativeElement, NativeField, NativeNode, NativeToken,
    SourceRange,
};
