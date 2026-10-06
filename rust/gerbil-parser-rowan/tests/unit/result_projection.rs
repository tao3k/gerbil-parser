use crate::{
    CaptureKind, CaptureSpec, EventCatalog, KindCategory, KindSpec, PreparedResultProfile,
    ProjectedNode, ProjectedValue, ProjectionInstruction, ProjectionOpcode, ResultCapture,
    ResultNodeSpec, ResultProfileSpec, ResultProjection, build_rowan_events_catalog,
};
#[path = "../fixtures/generated/result_projections.rs"]
mod generated;
fn fields(node: &ProjectedNode<'_>, output: &mut Vec<(&'static str, &'static str)>) {
    for child in node.children() {
        output.push((
            generated::RESULT_PROFILE.kinds[usize::from(node.kind())].name,
            child.field(),
        ));
        if let ProjectedValue::Node(node) = child.value() {
            fields(node, output);
        }
    }
}
#[test]
fn production_scheme_word_captures_have_identical_rust_projection_events_and_fields() {
    let profile = PreparedResultProfile::new(&generated::RESULT_PROFILE).unwrap();
    for (index, source) in generated::SOURCES.iter().enumerate() {
        let node = generated::replay(&profile, source, index).unwrap();
        let events = node.events();
        assert_eq!(events, generated::EXPECTED[index], "source={source:?}");
        let mut actual_fields = Vec::new();
        fields(&node, &mut actual_fields);
        assert_eq!(actual_fields, generated::FIELDS[index], "source={source:?}");
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
    }
}
#[test]
fn result_captures_reject_wrong_types_arity_ranges_and_foreign_owners() {
    static FOREIGN: ResultProfileSpec = ResultProfileSpec {
        kinds: generated::RESULT_PROFILE.kinds,
        nodes: generated::RESULT_PROFILE.nodes,
        projections: generated::RESULT_PROFILE.projections,
    };

    let profile = PreparedResultProfile::new(&generated::RESULT_PROFILE).unwrap();
    for captures in [
        vec![],
        vec![ResultCapture::Absent],
        vec![ResultCapture::Parts(vec![])],
        vec![ResultCapture::Span(1..2)],
        vec![ResultCapture::Span(0..3)],
    ] {
        assert!(profile.project("LiteralPart", "α", 0..2, captures).is_err());
    }
    assert!(profile.project("missing", "α", 0..2, vec![]).is_err());
    assert!(
        profile
            .project("Word", "α", 1..2, vec![ResultCapture::Parts(vec![])])
            .is_err()
    );
    let leaf = profile.bind("LiteralPart").unwrap();
    assert!(profile.bind("missing").is_err());
    let node = leaf
        .project("α", 0..2, vec![ResultCapture::Span(0..2)])
        .unwrap();
    assert_eq!(node.span(), 0..2);
    let foreign = PreparedResultProfile::new(&FOREIGN).unwrap();
    let node = foreign
        .project("LiteralPart", "α", 0..2, vec![ResultCapture::Span(0..2)])
        .unwrap();
    assert!(
        profile
            .project("Word", "α", 0..2, vec![ResultCapture::Parts(vec![node])])
            .is_err()
    );
    let first = String::from("α");
    let other = String::from("α");
    let node = profile
        .project("LiteralPart", &first, 0..2, vec![ResultCapture::Span(0..2)])
        .unwrap();
    assert!(
        profile
            .project("Word", &other, 0..2, vec![ResultCapture::Parts(vec![node])])
            .is_err()
    );
    let node = profile
        .project("LiteralPart", "αx", 0..2, vec![ResultCapture::Span(0..2)])
        .unwrap();
    assert!(
        profile
            .project("Word", "αx", 2..3, vec![ResultCapture::Parts(vec![node])])
            .is_err()
    );
}
#[test]
fn projection_admission_checks_unused_declarations_and_preserves_node_token_namespaces() {
    static EMPTY: ResultProfileSpec = ResultProfileSpec {
        kinds: &[],
        nodes: &[],
        projections: &[],
    };
    static KINDS: &[KindSpec] = &[
        KindSpec {
            name: "value",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "value",
            category: KindCategory::Token,
        },
    ];
    static NODES: &[ResultNodeSpec] = &[ResultNodeSpec {
        kind: 0,
        fields: &["text"],
    }];
    static CAPTURES: &[CaptureSpec] = &[CaptureSpec {
        name: "text",
        kind: CaptureKind::Span,
    }];
    static GOOD: ResultProfileSpec = ResultProfileSpec {
        kinds: KINDS,
        nodes: NODES,
        projections: &[ResultProjection {
            id: "leaf",
            kind: 0,
            captures: CAPTURES,
            instructions: &[ProjectionInstruction {
                field: "text",
                opcode: ProjectionOpcode::Token {
                    kind: 1,
                    required: true,
                },
            }],
        }],
    };
    static BAD: ResultProfileSpec = ResultProfileSpec {
        kinds: KINDS,
        nodes: NODES,
        projections: &[ResultProjection {
            id: "unused",
            kind: 0,
            captures: CAPTURES,
            instructions: &[ProjectionInstruction {
                field: "missing",
                opcode: ProjectionOpcode::Token {
                    kind: 1,
                    required: true,
                },
            }],
        }],
    };
    static WRONG: ResultProfileSpec = ResultProfileSpec {
        kinds: KINDS,
        nodes: NODES,
        projections: &[ResultProjection {
            id: "unused",
            kind: 0,
            captures: CAPTURES,
            instructions: &[ProjectionInstruction {
                field: "text",
                opcode: ProjectionOpcode::One { required: false },
            }],
        }],
    };
    let profile = PreparedResultProfile::new(&GOOD).unwrap();
    let node = profile
        .bind("leaf")
        .unwrap()
        .project("中", 0..3, vec![ResultCapture::Span(0..3)])
        .unwrap();
    assert_eq!(node.children()[0].field(), "text");
    let tree = build_rowan_events_catalog(
        &EventCatalog {
            root_kind: 0,
            kinds: GOOD.kinds,
        },
        "中",
        &node.events(),
    )
    .unwrap();
    assert_eq!(tree.to_string(), "中");
    assert!(PreparedResultProfile::new(&EMPTY).is_err());
    assert!(PreparedResultProfile::new(&BAD).is_err());
    assert!(PreparedResultProfile::new(&WRONG).is_err());
}
