use std::hint::black_box;
use std::time::{Duration, Instant};

use gerbil_parser_build_support::{
    AspRustScenario, AspRustScenarioObservation, ROWAN_ENGINE_DETERMINISTIC_HOT_PATH_SCENARIO_ID,
    ROWAN_ENGINE_EVENT_TREE_HOT_PATH_SCENARIO_ID, measure_asp_rust_scenario,
    render_asp_rust_scenario_benchmark_toml, rowan_engine_scenario_package,
};
use gerbil_parser_rowan_arithmetic as arithmetic_v1;

const ARITHMETIC_PARSE_COUNT: usize = 256;
const EVENT_PARSE_COUNT: usize = 128;
const EVENT_DEPTH: usize = 128;

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

    assert_red_zone(&scenario, &measurement, Duration::from_millis(10));
}

#[test]
#[ignore = "runner-sensitive ASP Rust performance Scenario; run focused in release mode"]
fn rowan_engine_event_tree_hot_path_v1() {
    use gerbil_parser_rowan::TreeEvent;

    let scenario = scenario(ROWAN_ENGINE_EVENT_TREE_HOT_PATH_SCENARIO_ID);
    let source = "x".repeat(EVENT_DEPTH);
    let mut events = Vec::with_capacity(2 * EVENT_DEPTH + EVENT_DEPTH + 2);
    events.push(TreeEvent::StartNode(arithmetic_v1::LANGUAGE.root_kind));
    for index in 0..EVENT_DEPTH {
        events.push(TreeEvent::StartNode(1));
        events.push(TreeEvent::Token {
            kind: 8,
            start: index,
            end: index + 1,
        });
    }
    events.extend(std::iter::repeat_n(TreeEvent::FinishNode, EVENT_DEPTH + 1));
    let sample =
        gerbil_parser_rowan::build_rowan_events(&arithmetic_v1::LANGUAGE, &source, &events)
            .expect("sample event stream must build");
    assert_eq!(
        gerbil_parser_rowan::SyntaxNode::new_root(sample)
            .descendants()
            .count(),
        EVENT_DEPTH + 1
    );

    let measurement = measure_asp_rust_scenario(&scenario, || {
        let started_at = Instant::now();
        for _ in 0..EVENT_PARSE_COUNT {
            let green = gerbil_parser_rowan::build_rowan_events(
                &arithmetic_v1::LANGUAGE,
                black_box(&source),
                black_box(&events),
            )
            .expect("generated structural events must form one lossless tree");
            black_box(green);
        }
        AspRustScenarioObservation::default()
            .with_timing("scheme_aot_events_to_rowan", started_at.elapsed())
            .with_metric("parse_count", EVENT_PARSE_COUNT as u64)
            .with_metric(
                "rowan_node_count",
                (EVENT_PARSE_COUNT * (EVENT_DEPTH + 1)) as u64,
            )
            .with_metric("provider_process_count", 0)
    })
    .expect("measure structural Rust/Rowan engine Scenario");

    assert_red_zone(&scenario, &measurement, Duration::from_millis(10));
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
