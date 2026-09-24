//! Keyed structural-line coverage over the shared AOT fixture.

use super::{KEY_LINE_STRUCTURE, KEY_LINES, LANGUAGE, key_line_references, parse_structural_lines};

#[test]
fn dynamic_keywords_and_heading_adjacent_planning_are_typed_and_lossless() {
    let source = "#+TITLE: α fixture\n* Heading\nSCHEDULED: <2026-09-24 Thu> DEADLINE: <2026-09-25 Fri>\nBody\nSCHEDULED: later prose\n";
    let root = parse_structural_lines(&LANGUAGE, &KEY_LINE_STRUCTURE, source)
        .unwrap()
        .syntax();
    assert_eq!(root.to_string(), source);
    let keywords: Vec<_> = root
        .descendants()
        .filter(|node| node.kind().0 == 38)
        .collect();
    let planning: Vec<_> = root
        .descendants()
        .filter(|node| node.kind().0 == 42)
        .collect();
    assert_eq!(keywords.len(), 1);
    assert_eq!(planning.len(), 1);
    assert_eq!(keywords[0].to_string(), "#+TITLE: α fixture\n");
    assert_eq!(
        planning[0].to_string(),
        "SCHEDULED: <2026-09-24 Thu> DEADLINE: <2026-09-25 Fri>\n"
    );
    let keys: Vec<_> = planning[0]
        .children_with_tokens()
        .filter_map(rowan::NodeOrToken::into_token)
        .filter(|token| token.kind().0 == 43)
        .map(|token| token.text().to_string())
        .collect();
    assert_eq!(keys, ["SCHEDULED", "DEADLINE"]);
    assert_eq!(
        root.descendants()
            .filter(|node| node.kind().0 == 42)
            .count(),
        1
    );
}

#[test]
fn invalid_key_rule_fails_closed() {
    let mut invalid = KEY_LINES[1];
    invalid.keys = &[];
    assert_eq!(
        key_line_references(invalid).unwrap_err().reason_kind,
        "invalid-structural-aot"
    );
}

#[test]
fn many_keyword_lines_preserve_linear_source_partitioning() {
    let source = "#+KEY: value\n".repeat(10_000);
    let root = parse_structural_lines(&LANGUAGE, &KEY_LINE_STRUCTURE, &source)
        .unwrap()
        .syntax();
    assert_eq!(root.to_string(), source);
    assert_eq!(
        root.children().filter(|node| node.kind().0 == 38).count(),
        10_000
    );
}
