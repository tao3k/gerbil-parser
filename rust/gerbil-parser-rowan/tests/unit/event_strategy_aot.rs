//! The Scheme fixture is executed by Gerbil tests and compiled to this Rust test.

#[path = "event_strategy_grammar.rs"]
mod grammar;
#[path = "event_strategy_generated.rs"]
mod strategy;

use gerbil_parser_rowan::{TreeEvent, parse_generated_events};

#[test]
fn scheme_aot_event_algorithm_builds_lossless_rowan_tree() {
    let source = "* α\r\nbody\n";
    let events = strategy::parse_event_lines(source);
    assert_eq!(
        events,
        vec![
            TreeEvent::StartNode(0),
            TreeEvent::StartNode(1),
            TreeEvent::Token {
                kind: 3,
                start: 0,
                end: 6
            },
            TreeEvent::FinishNode,
            TreeEvent::StartNode(2),
            TreeEvent::Token {
                kind: 3,
                start: 6,
                end: 11
            },
            TreeEvent::FinishNode,
            TreeEvent::FinishNode,
        ]
    );
    let parsed =
        parse_generated_events(&grammar::LANGUAGE, strategy::PARSER_DIGEST, source, &events)
            .expect("Scheme AOT events satisfy Rowan event contract");
    assert_eq!(parsed.syntax().to_string(), source);
    assert_eq!(
        parsed.receipt().parser_digest,
        Some(strategy::PARSER_DIGEST)
    );
    assert_eq!(parsed.syntax().children().count(), 2);
}

#[test]
fn scheme_aot_event_algorithm_handles_empty_source() {
    let source = "";
    let events = strategy::parse_event_lines(source);
    assert_eq!(events, vec![TreeEvent::StartNode(0), TreeEvent::FinishNode]);
    let parsed =
        parse_generated_events(&grammar::LANGUAGE, strategy::PARSER_DIGEST, source, &events)
            .expect("empty source forms a lossless document");
    assert_eq!(parsed.syntax().to_string(), "");
}
