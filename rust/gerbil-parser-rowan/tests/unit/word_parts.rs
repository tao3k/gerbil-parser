//! Production and inherited POO recipes are recognized independently in Rust.
#[allow(dead_code)]
#[path = "../fixtures/generated/word_parts.rs"]
mod generated;
use crate::{
    EventCatalog, PartGuard, PartOpcode, PartProfileSpec, PartRule, PreparedPartProfile,
    ProjectedNode, ProjectedValue, ResultProfileSpec, TreeEvent, build_rowan_events_catalog,
};
fn fields(
    root: &ProjectedNode<'_>,
    profile: &ResultProfileSpec,
) -> Vec<(&'static str, &'static str)> {
    let mut output = Vec::new();
    let mut stack: Vec<_> = root
        .children()
        .iter()
        .rev()
        .map(|child| (root, child))
        .collect();
    while let Some((parent, child)) = stack.pop() {
        output.push((
            profile.kinds[usize::from(parent.kind())].name,
            child.field(),
        ));
        if let ProjectedValue::Node(node) = child.value() {
            stack.extend(node.children().iter().rev().map(|child| (node, child)));
        }
    }
    output
}
#[test]
fn production_and_unicode_inheritance_match_scheme_events_fields_and_outcomes() {
    for (parts, results, regions, controls) in [
        (
            &generated::bash::PART_PROFILE,
            &generated::bash::RESULT_PROFILE,
            &generated::bash::regions::REGION,
            generated::bash::CONTROLS,
        ),
        (
            &generated::generic::PART_PROFILE,
            &generated::generic::RESULT_PROFILE,
            &generated::generic::regions::REGION,
            generated::generic::CONTROLS,
        ),
    ] {
        let engine = PreparedPartProfile::new(parts, results, regions).unwrap();
        for control in controls {
            let span = control.start..control.end;
            let result = match control.mode {
                "word" => engine.word(control.source, span).map(Some),
                "here" => engine.here_content(control.source, span).map(Some),
                "assignment" => engine.assignment(control.source, span),
                _ => panic!("unknown control"),
            };
            let status = match &result {
                Err(_) => 0,
                Ok(Some(_)) => 1,
                Ok(None) => 2,
            };
            assert_eq!(
                status,
                control.status,
                "{} {:?}: {:?}",
                control.mode,
                control.source,
                result.as_ref().err()
            );
            if let Ok(Some(node)) = result {
                assert_eq!(node.span(), control.start..control.end);
                let events = node.events();
                assert_eq!(
                    events, control.events,
                    "{} {:?}",
                    control.mode, control.source
                );
                assert_eq!(
                    fields(&node, results),
                    control.fields,
                    "{} {:?}",
                    control.mode,
                    control.source
                );
                let relative: Vec<_> = events
                    .into_iter()
                    .map(|event| match event {
                        TreeEvent::Token { kind, start, end } => TreeEvent::Token {
                            kind,
                            start: start - control.start,
                            end: end - control.start,
                        },
                        event => event,
                    })
                    .collect();
                let text = &control.source[control.start..control.end];
                let tree = build_rowan_events_catalog(
                    &EventCatalog {
                        root_kind: node.kind(),
                        kinds: results.kinds,
                    },
                    text,
                    &relative,
                )
                .unwrap();
                assert_eq!(tree.to_string(), text);
            }
        }
    }
}
#[test]
fn admission_rejects_rule_region_binding_and_projection_conflicts() {
    static UNKNOWN: PartProfileSpec = PartProfileSpec {
        assignment: Some(usize::MAX),
        ..generated::bash::PART_PROFILE
    };
    static RULES: &[PartRule] = &[PartRule {
        contexts: &["word"],
        prefix: "${",
        guard: PartGuard::Any,
        opcode: PartOpcode::Parameter { binding: 0 },
        projection: "Word",
        opening: 2,
        closing: 1,
    }];
    static SIGNATURE: PartProfileSpec = PartProfileSpec {
        rules: RULES,
        ..generated::bash::PART_PROFILE
    };
    static BAD_OPENING: PartProfileSpec = PartProfileSpec {
        rules: &[PartRule {
            opening: 1,
            ..RULES[0]
        }],
        ..generated::bash::PART_PROFILE
    };
    static VALID_RULE: PartRule = PartRule {
        projection: "ParameterExpansion",
        opcode: PartOpcode::Parameter { binding: 1 },
        ..RULES[0]
    };
    static DUPLICATE: PartProfileSpec = PartProfileSpec {
        rules: &[VALID_RULE, VALID_RULE],
        ..generated::bash::PART_PROFILE
    };
    static NULLABLE: PartProfileSpec = PartProfileSpec {
        bindings: &[crate::BindingSpec {
            name: &crate::TextProfile::Literal(""),
            prefixes: &[],
            operators: &[],
            subscript: None,
        }],
        assignment: None,
        ..generated::bash::PART_PROFILE
    };
    static BAD_SCOPE: PartProfileSpec = PartProfileSpec {
        bindings: &[
            generated::bash::PART_PROFILE.bindings[0],
            crate::BindingSpec {
                subscript: Some(crate::SubscriptSpec {
                    scopes: &[],
                    ..generated::bash::PART_PROFILE.bindings[1].subscript.unwrap()
                }),
                ..generated::bash::PART_PROFILE.bindings[1]
            },
            generated::bash::PART_PROFILE.bindings[2],
        ],
        ..generated::bash::PART_PROFILE
    };
    static WRONG_PAIR: PartProfileSpec = PartProfileSpec {
        rules: &[PartRule {
            opcode: PartOpcode::Pair,
            closing: 2,
            ..VALID_RULE
        }],
        ..generated::bash::PART_PROFILE
    };
    for spec in [
        &UNKNOWN,
        &SIGNATURE,
        &BAD_OPENING,
        &DUPLICATE,
        &NULLABLE,
        &BAD_SCOPE,
        &WRONG_PAIR,
    ] {
        assert!(
            PreparedPartProfile::new(
                spec,
                &generated::bash::RESULT_PROFILE,
                &generated::bash::regions::REGION
            )
            .is_err()
        );
    }
}
#[test]
fn bounded_source_and_deep_composition_preserve_full_source_ownership() {
    let engine = PreparedPartProfile::new(
        &generated::bash::PART_PROFILE,
        &generated::bash::RESULT_PROFILE,
        &generated::bash::regions::REGION,
    )
    .unwrap();
    assert!(engine.word("α", 1..2).is_err());
    assert!(engine.assignment("α", 0..1).is_err());
    assert!(engine.word("\"α\"tail", 0..3).is_err());
    let text = format!("{}中{}", "${x:-".repeat(512), "}".repeat(512));
    let node = engine.word(&text, 0..text.len()).unwrap();
    let events = node.events();
    let tree = build_rowan_events_catalog(
        &EventCatalog {
            root_kind: node.kind(),
            kinds: generated::bash::RESULT_PROFILE.kinds,
        },
        &text,
        &events,
    )
    .unwrap();
    assert_eq!(tree.to_string(), text);
}
