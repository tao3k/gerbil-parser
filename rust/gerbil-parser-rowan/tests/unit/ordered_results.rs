//! Scheme catalog-constructor parity for ordered repeated command fields.
#[path = "../fixtures/generated/ordered_results.rs"]
mod generated;
use crate::{
    EventCatalog, KindCategory, PreparedResultProfile, ProjectedValue, ResultChildCapture,
    ResultProfileSpec, build_rowan_events_catalog,
};
#[test]
fn ordered_command_construction_matches_scheme_and_lossless_text() {
    let profile = PreparedResultProfile::new(&generated::RESULT_PROFILE).unwrap();
    for (at, source) in generated::SOURCES.iter().enumerate() {
        let node = match at {
            0 => generated::replay_0(&profile, source),
            1 => generated::replay_1(&profile, source),
            2 => generated::replay_2(&profile, source),
            3 => generated::replay_3(&profile, source),
            _ => unreachable!(),
        }
        .unwrap();
        let events = node.events();
        assert_eq!(events, generated::EVENTS[at]);
        let tree = build_rowan_events_catalog(
            &EventCatalog {
                root_kind: node.kind(),
                kinds: generated::RESULT_PROFILE.kinds,
            },
            source,
            &events,
        )
        .unwrap();
        assert_eq!(tree.to_string(), *source);
        if at == 2 {
            let ProjectedValue::Node(command) = node.children()[0].value() else {
                panic!("command node");
            };
            assert_eq!(
                command
                    .children()
                    .iter()
                    .map(crate::ProjectedChild::field)
                    .collect::<Vec<_>>(),
                ["argument", "name", "argument"]
            );
        }
    }
}
#[test]
fn catalog_fields_utf8_and_owner_constraints_apply_before_publication() {
    static FOREIGN: ResultProfileSpec = ResultProfileSpec {
        ..generated::RESULT_PROFILE
    };
    let profile = PreparedResultProfile::new(&generated::RESULT_PROFILE).unwrap();
    let other = PreparedResultProfile::new(&FOREIGN).unwrap();
    let word = profile.bind_node("Word").unwrap();
    let source = String::from("α");
    let equal = String::from("α");
    let token = u16::try_from(
        generated::RESULT_PROFILE
            .kinds
            .iter()
            .position(|k| k.category == KindCategory::Token)
            .unwrap(),
    )
    .unwrap();
    assert!(profile.bind_node("unknown").is_err());
    assert!(word.build(&source, 0..1, vec![]).is_err());
    assert!(
        word.build(
            &source,
            0..2,
            vec![ResultChildCapture {
                field: "foreign",
                value: ProjectedValue::Token {
                    kind: token,
                    span: 0..2
                }
            }]
        )
        .is_err()
    );
    assert!(
        word.build(
            &source,
            0..2,
            vec![ResultChildCapture {
                field: "part",
                value: ProjectedValue::Token {
                    kind: 0,
                    span: 0..2
                }
            }]
        )
        .is_err()
    );
    assert!(
        word.build(
            &source,
            0..2,
            vec![ResultChildCapture {
                field: "part",
                value: ProjectedValue::Token {
                    kind: token,
                    span: 0..1
                }
            }]
        )
        .is_err()
    );
    let foreign = other
        .bind_node("Word")
        .unwrap()
        .build(&source, 0..2, vec![])
        .unwrap();
    assert!(
        word.build(
            &source,
            0..2,
            vec![ResultChildCapture {
                field: "part",
                value: ProjectedValue::Node(foreign)
            }]
        )
        .is_err()
    );
    let detached = word.build(&equal, 0..2, vec![]).unwrap();
    assert!(
        word.build(
            &source,
            0..2,
            vec![ResultChildCapture {
                field: "part",
                value: ProjectedValue::Node(detached)
            }]
        )
        .is_err()
    );
    let beyond = word.build(&source, 0..2, vec![]).unwrap();
    assert!(
        word.build(
            &source,
            0..0,
            vec![ResultChildCapture {
                field: "part",
                value: ProjectedValue::Node(beyond)
            }]
        )
        .is_err()
    );
}
