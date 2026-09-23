//! List-specific structural cases over the shared test language fixture.

use super::{LANGUAGE, LIST_STRUCTURE, parse_structural_lines};

#[test]
fn list_strategy_nests_items_and_preserves_continuations_and_ordered_markers() {
    let source = "* Heading\n- first [[id:one]]\n  continuation\n  - child\n  - next\n- second\n\n\n1. ordered\n2) next\n";
    let root = parse_structural_lines(&LANGUAGE, &LIST_STRUCTURE, source)
        .unwrap()
        .syntax();
    assert_eq!(root.to_string(), source);
    let lists: Vec<_> = root
        .descendants()
        .filter(|node| node.kind().0 == 34)
        .collect();
    assert_eq!(lists.len(), 3);
    assert_eq!(lists[1].parent().unwrap().kind().0, 35);
    assert_eq!(lists[0].parent().unwrap().kind().0, 1);
    assert_eq!(lists[2].parent().unwrap().kind().0, 1);
    assert_eq!(
        root.descendants()
            .filter(|node| node.kind().0 == 35)
            .count(),
        6
    );
    assert_eq!(
        root.descendants()
            .filter(|node| node.kind().0 == 19)
            .count(),
        1
    );
    let bullets: Vec<_> = root
        .descendants_with_tokens()
        .filter_map(rowan::NodeOrToken::into_token)
        .filter(|token| token.kind().0 == 36)
        .map(|token| token.text().to_string())
        .collect();
    assert_eq!(bullets, ["-", "-", "-", "-", "1.", "2)"]);
}

#[test]
fn list_indentation_uses_visual_tab_columns_and_rejects_multi_letter_counters() {
    let source = "- outer\n\t- tab item\n        - same-level item\nabc) prose\nx) ordered\n";
    let root = parse_structural_lines(&LANGUAGE, &LIST_STRUCTURE, source)
        .unwrap()
        .syntax();
    assert_eq!(root.to_string(), source);
    let lists: Vec<_> = root
        .descendants()
        .filter(|node| node.kind().0 == 34)
        .collect();
    assert_eq!(lists.len(), 3);
    assert_eq!(lists[1].children().count(), 2);
    let bullets: Vec<_> = root
        .descendants_with_tokens()
        .filter_map(rowan::NodeOrToken::into_token)
        .filter(|token| token.kind().0 == 36)
        .map(|token| token.text().to_string())
        .collect();
    assert_eq!(bullets, ["-", "-", "-", "x)"]);
}

#[test]
fn many_sibling_items_use_one_list_without_quadratic_ancestry_walks() {
    let source = "- item\n".repeat(10_000);
    let root = parse_structural_lines(&LANGUAGE, &LIST_STRUCTURE, &source)
        .unwrap()
        .syntax();
    assert_eq!(root.to_string(), source);
    let list = root
        .descendants()
        .find(|node| node.kind().0 == 34)
        .expect("one list");
    assert_eq!(list.children().count(), 10_000);
}
