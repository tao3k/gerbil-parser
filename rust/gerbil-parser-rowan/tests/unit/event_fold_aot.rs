//! Execute the stateful Scheme event fold after typed AOT lowering.

use super::event_strategy_aot_tests::grammar;

mod generated {
    use gerbil_parser_rowan::TreeEvent;
    include!("event_fold_generated.rs");
}

use gerbil_parser_rowan::{TreeEvent, parse_generated_events};

#[test]
fn scheme_event_fold_builds_lossless_multiline_rowan_tree() {
    let source = "a\nb\n* α\r\nc";
    let events = generated::parse_fold_lines(source);
    assert_eq!(
        events,
        vec![
            TreeEvent::StartNode(0),
            TreeEvent::StartNode(2),
            TreeEvent::Token {
                kind: 3,
                start: 0,
                end: 2,
            },
            TreeEvent::Token {
                kind: 3,
                start: 2,
                end: 4,
            },
            TreeEvent::FinishNode,
            TreeEvent::StartNode(1),
            TreeEvent::Token {
                kind: 3,
                start: 4,
                end: 10,
            },
            TreeEvent::FinishNode,
            TreeEvent::StartNode(2),
            TreeEvent::Token {
                kind: 3,
                start: 10,
                end: 11,
            },
            TreeEvent::FinishNode,
            TreeEvent::FinishNode,
        ]
    );
    let parsed = parse_generated_events(
        &grammar::LANGUAGE,
        generated::PARSER_DIGEST,
        source,
        &events,
    )
    .expect("Scheme event fold satisfies the Rowan event contract");
    assert_eq!(parsed.syntax().to_string(), source);
    assert_eq!(parsed.syntax().children().count(), 3);
    assert_eq!(
        parsed.receipt().parser_digest,
        Some(generated::PARSER_DIGEST)
    );
}

#[test]
fn scheme_event_fold_handles_empty_source() {
    let events = generated::parse_fold_lines("");
    assert_eq!(events, vec![TreeEvent::StartNode(0), TreeEvent::FinishNode]);
    let parsed = parse_generated_events(&grammar::LANGUAGE, generated::PARSER_DIGEST, "", &events)
        .expect("empty source forms a lossless document");
    assert_eq!(parsed.syntax().to_string(), "");
}
