//! Generic traversal direction for indexed syntax access.
/// Entering or leaving an existing syntax element.
#[derive(Clone, Debug)]
pub enum WalkEvent<T> {
    Enter(T),
    Leave(T),
}
