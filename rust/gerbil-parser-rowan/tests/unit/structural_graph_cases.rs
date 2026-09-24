use super::{GRAPH, KEY_LINE_STRUCTURE, LANGUAGE, parse_structural_lines, project_syntax_graph};

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
        planning
            .fields
            .iter()
            .filter(|field| field.name == "key")
            .map(|field| field.value.as_str())
            .collect::<Vec<_>>(),
        ["SCHEDULED", "DEADLINE"]
    );
    assert_eq!(
        planning
            .fields
            .iter()
            .filter(|field| field.name == "value")
            .map(|field| field.value.as_str())
            .collect::<Vec<_>>(),
        ["<2026-09-24 Thu>", "<2026-09-25 Fri>"]
    );
}
