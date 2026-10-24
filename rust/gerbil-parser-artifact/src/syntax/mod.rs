//! Source-backed navigation for independently executing AOT products.
//! FFI clients use borrowed native views and never materialize this storage.
mod navigation;
mod storage;
mod walk;
pub use navigation::{SyntaxElement, SyntaxNode, SyntaxToken};
pub use storage::{IndexedEntry, SyntaxKind, SyntaxTree, TextRange, TextSize};
pub use walk::WalkEvent;
