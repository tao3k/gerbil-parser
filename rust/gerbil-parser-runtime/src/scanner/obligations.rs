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
    pub(super) fn equivalent(&self, other: &Self) -> bool {
        fn ordered(queue: &Obligations) -> impl Iterator<Item = &Obligation> {
            let front = std::iter::successors(queue.front.as_deref(), |link| link.next.as_deref());
            let mut back: Vec<_> =
                std::iter::successors(queue.back.as_deref(), |link| link.next.as_deref()).collect();
            back.reverse();
            front.chain(back).map(|link| link.value.as_ref())
        }
        self.length == other.length && ordered(self).eq(ordered(other))
    }
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

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn convergence_compares_fifo_values_across_stack_partitions() {
        fn marker(text: &str) -> Obligation {
            Obligation {
                marker: text.into(),
                strip_tabs: false,
                quoted: false,
            }
        }
        let mut partitioned = Obligations::default();
        for text in ["drop", "A", "B"] {
            partitioned.push_back(marker(text));
        }
        partitioned.pop_front();
        partitioned.push_back(marker("C"));
        let mut queued = Obligations::default();
        for text in ["A", "B", "C"] {
            queued.push_back(marker(text));
        }
        assert!(partitioned.equivalent(&queued));
        let mut swapped = Obligations::default();
        for text in ["A", "C", "B"] {
            swapped.push_back(marker(text));
        }
        assert!(!partitioned.equivalent(&swapped));
        assert_eq!(partitioned.len(), 3);
        assert_eq!(queued.pop_front().unwrap().marker, "A");
        assert_eq!(partitioned.pop_front().unwrap().marker, "A");
    }
}
