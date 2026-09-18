use std::hint::black_box;
use std::time::{Duration, Instant};

use gerbil_parser_build_support::{
    AspRustScenario, AspRustScenarioObservation, ROWAN_ENGINE_DETERMINISTIC_HOT_PATH_SCENARIO_ID,
    ROWAN_ENGINE_SELECTIVE_GLR_SCALE_SCENARIO_ID, measure_asp_rust_scenario,
    render_asp_rust_scenario_benchmark_toml, rowan_engine_scenario_package,
};
use gerbil_parser_rowan::generated::{arithmetic_v1, gql_iso_39075_2024};

const ARITHMETIC_PARSE_COUNT: usize = 256;
const GQL_CORPUS: &[&str] = &[
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/create_closed_graph_from_graph_type_double_colon.gql"
    )),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/create_closed_graph_from_graph_type_lexical.gql"
    )),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/create_closed_graph_from_nested_graph_type_double_colon.gql"
    )),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/create_graph.gql"
    )),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/create_schema.gql"
    )),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/insert_statement.gql"
    )),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/match_and_insert_example.gql"
    )),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/match_with_exists_predicate_braces.gql"
    )),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/match_with_exists_predicate_nested_match.gql"
    )),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/match_with_exists_predicate_parentheses.gql"
    )),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/session_set_graph_to_current_graph.gql"
    )),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/session_set_graph_to_current_property_graph.gql"
    )),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/session_set_property_as_value.gql"
    )),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../languages/gql/iso-39075-2024/corpus/opengql-1.9.0/session_set_time_zone.gql"
    )),
];

#[test]
#[ignore = "runner-sensitive ASP Rust performance Scenario; run focused in release mode"]
fn rowan_engine_deterministic_hot_path_v1() {
    let scenario = scenario(ROWAN_ENGINE_DETERMINISTIC_HOT_PATH_SCENARIO_ID);
    let source = "  alpha + 2 * (beta - -3) + gamma * (17 - delta)  \n";
    let measurement = measure_asp_rust_scenario(&scenario, || {
        let parse_started_at = Instant::now();
        let mut node_count = 0_u64;
        for _ in 0..ARITHMETIC_PARSE_COUNT {
            let parsed = gerbil_parser_rowan::parse(&arithmetic_v1::LANGUAGE, black_box(source))
                .expect("generated arithmetic engine must parse");
            node_count += parsed.syntax().descendants().count() as u64;
            black_box(parsed.syntax().text_range());
        }
        AspRustScenarioObservation::default()
            .with_timing("generated_tables_to_rowan", parse_started_at.elapsed())
            .with_metric("parse_count", ARITHMETIC_PARSE_COUNT as u64)
            .with_metric("rowan_node_count", node_count)
            .with_metric("provider_process_count", 0)
    })
    .expect("measure deterministic Rust/Rowan engine Scenario");

    assert_red_zone(&scenario, &measurement, Duration::from_millis(25));
}

#[test]
#[ignore = "runner-sensitive ASP Rust performance Scenario; run focused in release mode"]
fn rowan_engine_selective_glr_scale_v1() {
    let scenario = scenario(ROWAN_ENGINE_SELECTIVE_GLR_SCALE_SCENARIO_ID);
    let measurement = measure_asp_rust_scenario(&scenario, || {
        let parse_started_at = Instant::now();
        let mut node_count = 0_u64;
        let mut branch_count = 0_u64;
        for source in GQL_CORPUS {
            let parsed = gerbil_parser_rowan::parse(&gql_iso_39075_2024::LANGUAGE, source)
                .expect("generated GQL engine must parse pinned corpus");
            node_count += parsed.syntax().descendants().count() as u64;
            branch_count += parsed.selective_glr_receipt().branches_explored as u64;
            black_box(parsed.syntax().text_range());
        }
        AspRustScenarioObservation::default()
            .with_timing("generated_tables_to_rowan", parse_started_at.elapsed())
            .with_metric("source_count", GQL_CORPUS.len() as u64)
            .with_metric("rowan_node_count", node_count)
            .with_metric("selective_glr_branch_count", branch_count)
            .with_metric("provider_process_count", 0)
    })
    .expect("measure production-scale Rust/Rowan engine Scenario");

    assert_red_zone(&scenario, &measurement, Duration::from_millis(75));
}

fn scenario(name: &str) -> AspRustScenario {
    rowan_engine_scenario_package()
        .scenarios
        .into_iter()
        .find(|scenario| scenario.name == name)
        .unwrap_or_else(|| panic!("missing Rowan engine Scenario {name}"))
}

fn assert_red_zone(
    scenario: &AspRustScenario,
    measurement: &gerbil_parser_build_support::AspRustScenarioMeasurement,
    max_total: Duration,
) {
    let receipt = render_asp_rust_scenario_benchmark_toml(scenario, measurement)
        .expect("render ASP Rust Scenario receipt");
    eprintln!("{receipt}");
    assert!(
        measurement.total_p95 <= max_total,
        "{} p95 {:?} exceeds {:?}\n{}",
        scenario.name,
        measurement.total_p95,
        max_total,
        receipt
    );
}
