use super::{
    GRAPH, HEADING_FIELDS_STRUCTURE, KEY_LINE_STRUCTURE, LANGUAGE, STRUCTURE,
    parse_structural_lines, project_syntax_graph,
};
use crate::engine::graph_projection::{
    GraphFieldMode, GraphFieldRule, GraphNodeRule, GraphProjectionSpec,
};

static GRAPH_WITH_EMPTY_VALUE: GraphProjectionSpec = GraphProjectionSpec {
    grammar_digest: LANGUAGE.grammar_digest,
    projection_digest: "sha256:3333333333333333333333333333333333333333333333333333333333333333",
    rules: &[GraphNodeRule {
        syntax_kind: 0,
        category: "document",
        kind: "root",
        fields: &[GraphFieldRule {
            token_kind: 23,
            name: "value",
            mode: GraphFieldMode::AppendOrEmpty,
        }],
    }],
};

#[test]
fn graph_projection_preserves_declared_empty_fields_without_zero_length_tokens() {
    let empty = parse_structural_lines(&LANGUAGE, &STRUCTURE, "")
        .unwrap()
        .syntax();
    let empty_records = project_syntax_graph(&LANGUAGE, &GRAPH_WITH_EMPTY_VALUE, &empty).unwrap();
    assert_eq!(empty_records[0].field("value"), Some(""));

    let source = "* Parent\n";
    let populated = parse_structural_lines(&LANGUAGE, &HEADING_FIELDS_STRUCTURE, source)
        .unwrap()
        .syntax();
    let populated_records =
        project_syntax_graph(&LANGUAGE, &GRAPH_WITH_EMPTY_VALUE, &populated).unwrap();
    assert_eq!(populated_records[0].field("value"), Some("Parent"));
    assert_eq!(populated_records[0].values("value").count(), 1);
}

#[test]
fn graph_projection_preserves_repeated_fields_without_merging_neighbors() {
    let source = "* Task\nSCHEDULED: <2026-09-24 Thu> DEADLINE: <2026-09-25 Fri>\n";
    let root = parse_structural_lines(&LANGUAGE, &KEY_LINE_STRUCTURE, source)
        .unwrap()
        .syntax();
    let records = project_syntax_graph(&LANGUAGE, &GRAPH, &root).unwrap();
    let planning = records
        .iter()
        .find(|record| record.kind == "planning")
        .expect("planning graph record");
    assert_eq!(
        planning.values("key").collect::<Vec<_>>(),
        ["SCHEDULED", "DEADLINE"]
    );
    assert_eq!(
        planning.values("value").collect::<Vec<_>>(),
        ["<2026-09-24 Thu>", "<2026-09-25 Fri>"]
    );
}
