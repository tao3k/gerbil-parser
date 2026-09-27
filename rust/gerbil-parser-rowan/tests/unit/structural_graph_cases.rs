use super::{
    GRAPH, HEADING_FIELDS_STRUCTURE, KEY_LINE_STRUCTURE, LANGUAGE, STRUCTURE,
    parse_structural_lines, project_syntax_graph,
};
use crate::engine::graph_projection::{
    GraphFieldMode, GraphFieldRule, GraphNodeRule, GraphProjectionSpec,
};
use rowan::TextRange;

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

static GRAPH_WITH_NODE_TEXT: GraphProjectionSpec = GraphProjectionSpec {
    grammar_digest: LANGUAGE.grammar_digest,
    projection_digest: "sha256:4444444444444444444444444444444444444444444444444444444444444444",
    rules: &[
        GraphNodeRule {
            syntax_kind: 0,
            category: "document",
            kind: "root",
            fields: &[],
        },
        GraphNodeRule {
            syntax_kind: 1,
            category: "section",
            kind: "headline",
            fields: &[GraphFieldRule {
                token_kind: 2,
                name: "source-line",
                mode: GraphFieldMode::NodeText,
            }],
        },
    ],
};

static GRAPH_WITH_EACH_NODE_TEXT: GraphProjectionSpec = GraphProjectionSpec {
    grammar_digest: LANGUAGE.grammar_digest,
    projection_digest: "sha256:5555555555555555555555555555555555555555555555555555555555555555",
    rules: &[GraphNodeRule {
        syntax_kind: 0,
        category: "document",
        kind: "root",
        fields: &[GraphFieldRule {
            token_kind: 1,
            name: "section-source",
            mode: GraphFieldMode::EachNodeText,
        }],
    }],
};

#[test]
fn graph_projection_retains_each_descendant_node_text_in_source_order() {
    let source = "* First\n* Second\n";
    let root = parse_structural_lines(&LANGUAGE, &HEADING_FIELDS_STRUCTURE, source)
        .unwrap()
        .syntax();
    let records = project_syntax_graph(&LANGUAGE, &GRAPH_WITH_EACH_NODE_TEXT, &root).unwrap();
    assert_eq!(
        records[0].values("section-source").collect::<Vec<_>>(),
        ["* First\n", "* Second\n"]
    );
}

#[test]
fn graph_projection_reads_nested_node_text_without_reparsing_language_syntax() {
    let source = "* Parent\n";
    let root = parse_structural_lines(&LANGUAGE, &HEADING_FIELDS_STRUCTURE, source)
        .unwrap()
        .syntax();
    let records = project_syntax_graph(&LANGUAGE, &GRAPH_WITH_NODE_TEXT, &root).unwrap();
    let headline = records
        .iter()
        .find(|record| record.kind == "headline")
        .expect("projected section");
    assert_eq!(headline.field("source-line"), Some(source));
    assert_eq!(
        headline.field_range("source-line"),
        Some(TextRange::new(
            0.into(),
            u32::try_from(source.len()).unwrap().into(),
        ))
    );
}

#[test]
fn graph_projection_preserves_declared_empty_fields_without_zero_length_tokens() {
    let empty = parse_structural_lines(&LANGUAGE, &STRUCTURE, "")
        .unwrap()
        .syntax();
    let empty_records = project_syntax_graph(&LANGUAGE, &GRAPH_WITH_EMPTY_VALUE, &empty).unwrap();
    assert_eq!(empty_records[0].field("value"), Some(""));
    assert_eq!(
        empty_records[0].field_range("value"),
        Some(TextRange::empty(0.into()))
    );

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
    let value_ranges = planning
        .fields
        .iter()
        .filter(|field| field.name == "value")
        .map(|field| field.range)
        .collect::<Vec<_>>();
    let first = u32::try_from(source.find("<2026-09-24 Thu>").unwrap()).unwrap();
    let second = u32::try_from(source.find("<2026-09-25 Fri>").unwrap()).unwrap();
    assert_eq!(
        value_ranges,
        [
            TextRange::new(first.into(), (first + 16).into()),
            TextRange::new(second.into(), (second + 16).into()),
        ]
    );
}
