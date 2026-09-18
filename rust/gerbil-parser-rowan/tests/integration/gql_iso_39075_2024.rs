use gerbil_parser_rowan::generated::gql_iso_39075_2024::LANGUAGE;

static CORPUS: &[(&str, &str)] = &[
    (
        "create_closed_graph_from_graph_type_double_colon",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/",
            "create_closed_graph_from_graph_type_double_colon.gql"
        )),
    ),
    (
        "create_closed_graph_from_graph_type_lexical",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/",
            "create_closed_graph_from_graph_type_lexical.gql"
        )),
    ),
    (
        "create_closed_graph_from_nested_graph_type_double_colon",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/",
            "create_closed_graph_from_nested_graph_type_double_colon.gql"
        )),
    ),
    (
        "create_graph",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/create_graph.gql"
        )),
    ),
    (
        "create_schema",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/create_schema.gql"
        )),
    ),
    (
        "insert_statement",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/insert_statement.gql"
        )),
    ),
    (
        "match_and_insert_example",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/match_and_insert_example.gql"
        )),
    ),
    (
        "match_with_exists_predicate_braces",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/",
            "match_with_exists_predicate_braces.gql"
        )),
    ),
    (
        "match_with_exists_predicate_nested_match",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/",
            "match_with_exists_predicate_nested_match.gql"
        )),
    ),
    (
        "match_with_exists_predicate_parentheses",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/",
            "match_with_exists_predicate_parentheses.gql"
        )),
    ),
    (
        "session_set_graph_to_current_graph",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/",
            "session_set_graph_to_current_graph.gql"
        )),
    ),
    (
        "session_set_graph_to_current_property_graph",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/",
            "session_set_graph_to_current_property_graph.gql"
        )),
    ),
    (
        "session_set_property_as_value",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/",
            "session_set_property_as_value.gql"
        )),
    ),
    (
        "session_set_time_zone",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/session_set_time_zone.gql"
        )),
    ),
];

#[test]
fn pinned_opengql_corpus_roundtrips_through_generated_selective_glr() {
    let mut explored = 0;
    let mut merged = 0;
    for (name, source) in CORPUS {
        let parsed = gerbil_parser_rowan::parse(&LANGUAGE, source)
            .unwrap_or_else(|error| panic!("{name}: {error:#?}"));
        assert_eq!(parsed.syntax().to_string(), *source, "{name}");
        explored += parsed.selective_glr_receipt().branches_explored;
        merged += parsed.selective_glr_receipt().merged_branches;
    }
    assert!(explored > 0, "corpus must exercise generated fork tables");
    assert!(
        merged > 0,
        "corpus must prove equivalent completion merging"
    );
}

#[test]
fn generated_gql_product_carries_the_pinned_authority() {
    assert_eq!(LANGUAGE.language, "gql");
    assert_eq!(LANGUAGE.version, "edition-1-2024-04");
    assert_eq!(
        LANGUAGE.contract,
        "iso-iec-39075-2024.opengql-1.9.0-syntax.v1"
    );
    assert_eq!(LANGUAGE.actions.len(), 2_834);
    assert_eq!(LANGUAGE.gotos.len(), 2_834);
}
