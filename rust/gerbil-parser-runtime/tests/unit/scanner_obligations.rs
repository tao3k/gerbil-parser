//! Ordered convergence compares queue values without changing either checkpoint.
use super::{Obligation, Obligations};

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
