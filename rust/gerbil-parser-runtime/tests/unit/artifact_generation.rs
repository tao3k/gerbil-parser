//! Canonical Scheme GPA1 bytes consumed through the same generated language.
use crate::records_contextual_fixture::generated as language;
use crate::{SyntaxNode, parse, parse_contextual};
use gerbil_parser_artifact::{
    NativeArtifactView, NativeCatalog, NativeElement, NativeEventKind, NativeNode,
};
#[path = "../fixtures/artifact_generated.rs"]
mod native;

fn catalog(contextual: bool) -> &'static NativeCatalog {
    static CANONICAL: std::sync::OnceLock<NativeCatalog> = std::sync::OnceLock::new();
    static CONTEXTUAL: std::sync::OnceLock<NativeCatalog> = std::sync::OnceLock::new();
    if contextual {
        CONTEXTUAL
            .get_or_init(|| NativeCatalog::from_descriptor(native::CONTEXTUAL_DESCRIPTOR).unwrap())
    } else {
        CANONICAL.get_or_init(|| NativeCatalog::from_descriptor(native::DESCRIPTOR).unwrap())
    }
}
fn decode<'a>(
    payload: &'a [u8],
    source: &'a str,
) -> Result<NativeArtifactView<'a>, gerbil_parser_artifact::NativeArtifactError> {
    NativeArtifactView::decode(payload, source, catalog(false))
}
fn compare(native: NativeNode<'_, '_>, aot: &SyntaxNode, source: &str) {
    let kind = language::LANGUAGE.kinds[aot.kind().0 as usize].name;
    assert_eq!(native.kind(), kind);
    let range = aot.text_range();
    assert_eq!(native.range().start, usize::from(range.start()));
    assert_eq!(native.range().end, usize::from(range.end()));
    assert_eq!(native.text(), aot.text());
    assert_eq!(
        native.text().as_ptr(),
        source[native.range().start..].as_ptr()
    );
    let left: Vec<_> = native.children().collect();
    let right: Vec<_> = aot.children_with_tokens().collect();
    assert_eq!(left.len(), right.len());
    for (left, right) in left.into_iter().zip(right) {
        match left {
            NativeElement::Node(node) => {
                assert_eq!(node.parent().unwrap().id(), native.id());
                compare(node, &right.into_node().unwrap(), source);
            }
            NativeElement::Token(token) => {
                let right = right.into_token().unwrap();
                assert_eq!(
                    token.kind(),
                    language::LANGUAGE.kinds[right.kind().0 as usize].name
                );
                assert_eq!(token.text(), right.text());
                assert_eq!(token.range().start, usize::from(right.text_range().start()));
                assert_eq!(token.range().end, usize::from(right.text_range().end()));
                assert_eq!(
                    token.text().as_ptr(),
                    source[token.range().start..].as_ptr()
                );
            }
        }
    }
    for field in native.fields() {
        assert!(catalog(false).fields().iter().any(|f| f == field.name()));
        assert_eq!(
            field.text().as_ptr(),
            source[field.range().start..].as_ptr()
        );
    }
}
#[test]
fn scheme_native_ast_matches_independent_aot_without_tree_conversion() {
    for &(source, payload) in native::CASES {
        let source = std::str::from_utf8(source).expect("Scheme source");
        let view = decode(payload, source).expect("canonical GPA1");
        let expected = parse(&language::LANGUAGE, source);
        assert_eq!(view.accepted(), expected.is_ok());
        if let Ok(expected) = expected {
            let actual = view.root().unwrap();
            compare(actual, &expected.syntax(), source);
            assert_eq!(actual.text(), source);
            let contextual =
                parse_contextual(&language::CONTEXTUAL, source).expect("shared scanner");
            compare(actual, &contextual.syntax(), source);
        } else {
            assert!(view.root().is_none());
        }
        for token in view
            .events()
            .filter(|event| event.kind == NativeEventKind::Token)
        {
            let text = view.token_text(token).expect("source slice");
            assert_eq!(text.as_ptr(), source[token.start..].as_ptr());
        }
    }
}

#[test]
fn truncated_extended_and_incompatible_payloads_fail_before_iteration() {
    let (source, payload) = native::CASES[1];
    let source = std::str::from_utf8(source).unwrap();
    for length in 0..payload.len() {
        assert!(decode(&payload[..length], source).is_err());
    }
    let mut extended = payload.to_vec();
    extended.push(0);
    assert!(decode(&extended, source).is_err());
    for offset in [0, 4, 8, 12, 16, 48] {
        let mut bad = payload.to_vec();
        bad[offset] ^= 255;
        assert!(decode(&bad, source).is_err(), "header offset {offset}");
    }
    assert_eq!(
        decode(payload, "b=1\n").unwrap_err().reason,
        "source-digest"
    );
    let wrong = String::from_utf8(native::DESCRIPTOR.to_vec())
        .unwrap()
        .replace(
            catalog(false).grammar_digest(),
            "sha256:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        );
    let wrong = NativeCatalog::from_descriptor(wrong.as_bytes()).unwrap();
    assert!(NativeArtifactView::decode(payload, source, &wrong).is_err());
}

#[test]
fn record_mutations_reject_unknown_tags_symbols_ids_ranges_and_nesting() {
    let (source, payload) = native::CASES[1];
    let source = std::str::from_utf8(source).unwrap();
    for (index, event) in decode(payload, source).unwrap().events().enumerate() {
        for field in [0, 4, 8, 16, 20] {
            let mut bad = payload.to_vec();
            bad[80 + index * 24 + field..80 + index * 24 + field + 4]
                .copy_from_slice(&u32::MAX.to_le_bytes());
            assert!(
                decode(&bad, source).is_err(),
                "event {index} field {field} {:?}",
                event.kind
            );
        }
    }
    let mut bad = native::DESCRIPTOR.to_vec();
    bad[0] = b'!';
    assert!(NativeCatalog::from_descriptor(&bad).is_err());
}

#[test]
fn borrowed_views_keep_parallel_source_and_payload_lifetimes_independent() {
    std::thread::scope(|scope| {
        for &(source, payload) in native::CASES {
            scope.spawn(move || {
                let source = std::str::from_utf8(source).unwrap();
                for _ in 0..16 {
                    let view = decode(payload, source).unwrap();
                    assert_eq!(view.events().len(), (payload.len() - 80) / 24);
                    if view.accepted() {
                        assert_eq!(view.root().unwrap().text(), source);
                    }
                }
            });
        }
    });
}

#[test]
fn syntax_status_utf8_boundaries_and_field_identity_are_checked() {
    let (source, payload) = native::CASES[1];
    let source = std::str::from_utf8(source).unwrap();
    let mut rejected = payload.to_vec();
    rejected[8] = 1;
    assert!(decode(&rejected, source).is_err());
    let view = decode(payload, source).unwrap();
    for (index, event) in view.events().enumerate() {
        if event.kind == NativeEventKind::Token && event.start == 0 {
            let mut split = payload.to_vec();
            split[80 + index * 24 + 20..80 + index * 24 + 24].copy_from_slice(&1_u32.to_le_bytes());
            assert!(decode(&split, source).is_err());
        }
        if event.kind == NativeEventKind::FinishField {
            let mut mismatched = payload.to_vec();
            let other = (event.symbol + 1) % u32::try_from(native::FIELD_COUNT).unwrap();
            mismatched[80 + index * 24 + 4..80 + index * 24 + 8]
                .copy_from_slice(&other.to_le_bytes());
            assert!(decode(&mismatched, source).is_err());
        }
    }
}

#[test]
fn contextual_native_identity_matches_its_product_and_rejects_canonical_binding() {
    for &(source, payload) in native::CONTEXTUAL_CASES {
        let source = std::str::from_utf8(source).unwrap();
        let view = NativeArtifactView::decode(payload, source, catalog(true))
            .expect("contextual native GPA1");
        assert_eq!(
            decode(payload, source).unwrap_err().reason,
            "grammar-digest"
        );
        let expected = parse_contextual(&language::CONTEXTUAL, source);
        assert_eq!(view.accepted(), expected.is_ok());
        if let Ok(expected) = expected {
            compare(view.root().unwrap(), &expected.syntax(), source);
        } else {
            assert!(view.root().is_none());
        }
    }
    let (source, payload) = native::CASES[1];
    assert!(
        NativeArtifactView::decode(payload, std::str::from_utf8(source).unwrap(), catalog(true))
            .is_err()
    );
}

#[test]
fn ast_fields_retain_names_values_order_and_declared_ownership() {
    let (source, payload) = native::CASES[2];
    let source = std::str::from_utf8(source).unwrap();
    let view = decode(payload, source).unwrap();
    let assignments: Vec<_> = view
        .root()
        .unwrap()
        .children()
        .filter_map(|child| match child {
            NativeElement::Node(node) => Some(node),
            NativeElement::Token(_) => None,
        })
        .collect();
    assert_eq!(assignments.len(), 2);
    for (assignment, name, value) in [(&assignments[0], "a", "1"), (&assignments[1], "b", "\"β\"")]
    {
        assert_eq!(
            assignment
                .field("name")
                .map(|field| field.text())
                .collect::<Vec<_>>(),
            [name]
        );
        assert_eq!(
            assignment
                .field("value")
                .map(|field| field.text())
                .collect::<Vec<_>>(),
            [value]
        );
        assert_eq!(
            assignment
                .fields()
                .map(|field| field.name())
                .collect::<Vec<_>>(),
            ["name", "value"]
        );
    }
    let invalid = u32::try_from(
        catalog(false)
            .fields()
            .iter()
            .position(|field| field == "text")
            .unwrap(),
    )
    .unwrap();
    let mut bad = payload.to_vec();
    for (index, event) in view.events().enumerate() {
        if matches!(
            event.kind,
            NativeEventKind::StartField | NativeEventKind::FinishField
        ) {
            bad[80 + index * 24 + 4..80 + index * 24 + 8].copy_from_slice(&invalid.to_le_bytes());
        }
    }
    assert_eq!(decode(&bad, source).unwrap_err().reason, "field-owner");
}

#[test]
fn repeated_zero_width_and_absent_fields_follow_canonical_associations() {
    let catalog = NativeCatalog::from_descriptor(native::SCOPES_DESCRIPTOR).unwrap();
    for &(source, payload) in native::SCOPES_CASES {
        let source = std::str::from_utf8(source).unwrap();
        let view = NativeArtifactView::decode(payload, source, &catalog).unwrap();
        let root = view.root().unwrap();
        let items: Vec<_> = root.field("item").collect();
        assert_eq!(items.len(), source.len());
        for (offset, field) in items.iter().enumerate() {
            assert_eq!(field.text(), "x");
            assert_eq!(field.range().start, offset);
            assert_eq!(field.range().end, offset + 1);
            assert_eq!(field.children().count(), 1);
        }
        let marker: Vec<_> = root.field("marker").collect();
        assert_eq!(marker.len(), 1);
        assert_eq!(marker[0].text(), "");
        assert_eq!(marker[0].range().start, source.len());
        assert_eq!(marker[0].range().end, source.len());
        assert_eq!(marker[0].children().count(), 1);
        assert_eq!(root.field("absent").count(), 0);
        assert_eq!(root.fields().count(), items.len() + 1);
    }
}

#[test]
fn accepted_and_rejected_tokens_preserve_scanner_identity_without_an_ast() {
    for (contextual, cases) in [(false, native::CASES), (true, native::CONTEXTUAL_CASES)] {
        for &(source, payload) in cases {
            let source = std::str::from_utf8(source).unwrap();
            let catalog = catalog(contextual);
            let view = NativeArtifactView::decode(payload, source, catalog).unwrap();
            let events: Vec<_> = view
                .events()
                .filter(|e| e.kind == NativeEventKind::Token)
                .collect();
            let tokens: Vec<_> = view.tokens().collect();
            assert_eq!(tokens.len(), events.len());
            let mut offset = 0;
            for (ordinal, (token, event)) in tokens.iter().zip(events).enumerate() {
                assert_eq!(token.id(), u64::try_from(ordinal).unwrap());
                assert_eq!(
                    token.class(),
                    catalog.terminals()[event.symbol as usize].name
                );
                assert_eq!(token.range().start, offset);
                assert_eq!(token.range().end, event.end);
                assert_eq!(token.text(), &source[offset..event.end]);
                assert_eq!(token.text().as_ptr(), source[offset..].as_ptr());
                assert_eq!(token.parent().is_some(), view.accepted());
                offset = event.end;
            }
            assert_eq!(offset, source.len());
        }
    }
    let catalog = NativeCatalog::from_descriptor(native::SCOPES_DESCRIPTOR).unwrap();
    let (source, payload) = native::SCOPES_CASES[1];
    let view = NativeArtifactView::decode(payload, std::str::from_utf8(source).unwrap(), &catalog)
        .unwrap();
    for token in view.tokens() {
        assert_eq!(token.class(), "item-token");
        assert_eq!(token.kind(), "Identifier");
    }
}
