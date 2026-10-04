//! Persistent two-stack FIFO: checkpoints share marker storage in O(1).
use super::Obligation;
use std::sync::Arc;
#[derive(Debug)]
struct Link {
    value: Arc<Obligation>,
    next: Option<Arc<Self>>,
}
impl Drop for Link {
    fn drop(&mut self) {
        // Drop long uniquely owned tails iteratively, avoiding call-stack growth.
        let mut tail = self.next.take();
        while let Some(link) = tail {
            let Ok(mut owned) = Arc::try_unwrap(link) else {
                break;
            };
            tail = owned.next.take();
        }
    }
}
#[derive(Clone, Debug, Default)]
pub(super) struct Obligations {
    front: Option<Arc<Link>>,
    back: Option<Arc<Link>>,
    length: usize,
}
impl Obligations {
    pub(super) fn len(&self) -> usize {
        self.length
    }
    pub(super) fn is_empty(&self) -> bool {
        self.length == 0
    }
    pub(super) fn push_back(&mut self, value: Obligation) {
        self.back = Some(Arc::new(Link {
            value: Arc::new(value),
            next: self.back.take(),
        }));
        self.length += 1;
    }
    pub(super) fn pop_front(&mut self) -> Option<Arc<Obligation>> {
        if self.front.is_none() {
            let mut back = self.back.take();
            while let Some(link) = back.take() {
                self.front = Some(Arc::new(Link {
                    value: Arc::clone(&link.value),
                    next: self.front.take(),
                }));
                back.clone_from(&link.next);
            }
        }
        let front = self.front.take()?;
        self.front.clone_from(&front.next);
        self.length -= 1;
        Some(Arc::clone(&front.value))
    }
}
