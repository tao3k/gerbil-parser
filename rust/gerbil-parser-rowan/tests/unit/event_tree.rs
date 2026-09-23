use super::event_tree::build_rowan_events;
use super::model::{KindCategory, KindSpec, LanguageSpec, TreeEvent};

static KINDS: &[KindSpec] = &[
    KindSpec {
        name: "Document",
        category: KindCategory::Node,
    },
    KindSpec {
        name: "Headline",
        category: KindCategory::Node,
    },
    KindSpec {
        name: "Section",
        category: KindCategory::Node,
    },
    KindSpec {
        name: "Line",
        category: KindCategory::Token,
    },
];
static LANGUAGE: LanguageSpec = LanguageSpec {
    language: "event-test",
    version: "v1",
    contract: "event-test.v1",
    grammar_digest: "sha256:0000000000000000000000000000000000000000000000000000000000000000",
    case_insensitive: false,
    root_kind: 0,
    kinds: KINDS,
    terminals: &[],
    lexical_rules: &[],
    actions: &[],
    gotos: &[],
    productions: &[],
};

#[test]
fn unbounded_outline_events_build_one_lossless_tree() {
    let source = "* Parent\n** 子\r\nbody\n";
    let events = [
        TreeEvent::StartNode(0),
        TreeEvent::StartNode(1),
        TreeEvent::Token {
            kind: 3,
            start: 0,
            end: 9,
        },
        TreeEvent::StartNode(1),
        TreeEvent::Token {
            kind: 3,
            start: 9,
            end: 17,
        },
        TreeEvent::StartNode(2),
        TreeEvent::Token {
            kind: 3,
            start: 17,
            end: source.len(),
        },
        TreeEvent::FinishNode,
        TreeEvent::FinishNode,
        TreeEvent::FinishNode,
        TreeEvent::FinishNode,
    ];
    let green = build_rowan_events(&LANGUAGE, source, &events).expect("valid nested events");
    let root = super::model::SyntaxNode::new_root(green);
    assert_eq!(root.to_string(), source);
    assert_eq!(root.descendants().count(), 4);
    assert_eq!(root.children().count(), 1);
    assert_eq!(root.first_child().unwrap().children().count(), 1);
}

#[test]
fn malformed_events_fail_closed() {
    let source = "é\n";
    let mut events = vec![
        TreeEvent::StartNode(0),
        TreeEvent::Token {
            kind: 3,
            start: 0,
            end: source.len(),
        },
        TreeEvent::FinishNode,
    ];
    assert!(build_rowan_events(&LANGUAGE, source, &events).is_ok());

    events[1] = TreeEvent::Token {
        kind: 3,
        start: 0,
        end: 1,
    };
    assert_eq!(
        build_rowan_events(&LANGUAGE, source, &events)
            .expect_err("split UTF-8 codepoint")
            .reason_kind,
        "event-range"
    );

    events[1] = TreeEvent::Token {
        kind: 1,
        start: 0,
        end: source.len(),
    };
    assert_eq!(
        build_rowan_events(&LANGUAGE, source, &events)
            .expect_err("node identity used as token")
            .reason_kind,
        "event-kind"
    );

    events[1] = TreeEvent::FinishNode;
    assert_eq!(
        build_rowan_events(&LANGUAGE, source, &events)
            .expect_err("double root finish")
            .reason_kind,
        "event-nesting"
    );
}
