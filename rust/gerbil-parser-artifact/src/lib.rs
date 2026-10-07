//! Rust access to Scheme-owned parser results, with no parser or tree dependency.
#![forbid(unsafe_code)]
mod admission;
mod catalog;
mod datum;
mod view;
mod wire;
pub use catalog::{KindCategory, NativeCatalog, NativeKind, NativeTerminal};
pub use view::{
    NativeArtifactView, NativeChildren, NativeElement, NativeField, NativeFields, NativeNode,
    NativeToken, SourceRange,
};
pub use wire::{NativeArtifactError, NativeEvent, NativeEventKind};
pub mod syntax;

#[cfg(test)]
#[path = "../tests/unit/language_alignment.rs"]
mod language_alignment_tests;
