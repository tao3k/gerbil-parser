//! Canonical Scheme GPA1 bytes consumed through the same generated language.
use super::{NativeArtifactView, NativeEventKind};
use crate::records_contextual_fixture as language;
use crate::{LanguageSpec, SyntaxNode, parse, parse_contextual};
#[path = "../fixtures/native_artifact_generated.rs"]
mod native;

static WRONG: LanguageSpec = LanguageSpec {
    grammar_digest: "sha256:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
    ..language::LANGUAGE
};

fn decode<'a>(
    payload: &'a [u8],
    source: &'a str,
) -> Result<NativeArtifactView<'a>, super::NativeArtifactError> {
    NativeArtifactView::decode(payload, source, &language::LANGUAGE, native::FIELD_COUNT)
}

#[test]
fn scheme_native_bytes_build_the_same_rowan_tree_as_rust_aot() {
    for &(source, payload) in native::CASES {
        let source = std::str::from_utf8(source).expect("Scheme source");
        let view = decode(payload, source).expect("canonical GPA1");
        let expected = parse(&language::LANGUAGE, source);
        assert_eq!(view.accepted(), expected.is_ok());
        if let Ok(expected) = expected {
            let actual = SyntaxNode::new_root(view.to_rowan().expect("accepted native tree"));
            assert_eq!(format!("{actual:#?}"), format!("{:#?}", expected.syntax()));
            assert_eq!(actual.to_string(), source);
            let contextual =
                parse_contextual(&language::CONTEXTUAL, source).expect("shared scanner");
            assert_eq!(
                format!("{actual:#?}"),
                format!("{:#?}", contextual.syntax())
            );
        } else {
            assert_eq!(
                view.to_rowan().expect_err("rejected tree").reason,
                "rejected-syntax"
            );
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
    assert!(NativeArtifactView::decode(payload, source, &WRONG, native::FIELD_COUNT).is_err());
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
    assert!(NativeArtifactView::decode(payload, source, &language::LANGUAGE, 0).is_err());
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
                        assert_eq!(
                            SyntaxNode::new_root(view.to_rowan().unwrap()).to_string(),
                            source
                        );
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
