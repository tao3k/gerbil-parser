//! Borrowed GPA1 transport views for compiled Scheme language packs.
mod admission;
mod view;
mod wire;

pub use view::NativeArtifactView;
pub use wire::{NativeArtifactError, NativeEvent, NativeEventKind};

#[cfg(test)]
#[path = "../../tests/unit/native_artifact.rs"]
mod tests;
