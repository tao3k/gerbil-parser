use gerbil_parser_runtime::{Diagnostic, EventCatalog, KindCategory, KindSpec, TreeEvent};
use gerbil_parser_runtime_external::{
    ExternalLanguageSpec, ExternalParseError, parse_external_events,
};

static KINDS: &[KindSpec] = &[
    KindSpec {
        name: "Document",
        category: KindCategory::Node,
    },
    KindSpec {
        name: "Text",
        category: KindCategory::Token,
    },
];
static EXTERNAL: ExternalLanguageSpec = ExternalLanguageSpec {
    language: "independent-text",
    version: "edition-a",
    contract: "independent-text.syntax-a",
    provider: "independent-parser",
    provider_version: "0.2",
    provider_digest: "sha256:1111111111111111111111111111111111111111111111111111111111111111",
    catalog: EventCatalog {
        root_kind: 0,
        kinds: KINDS,
    },
};
static INVALID: ExternalLanguageSpec = ExternalLanguageSpec {
    provider_digest: "stale",
    ..EXTERNAL
};

#[test]
fn independent_provider_builds_lossless_tree_with_external_receipt() {
    let source = "é\n";
    let events = [
        TreeEvent::StartNode(0),
        TreeEvent::Token {
            kind: 1,
            start: 0,
            end: source.len(),
        },
        TreeEvent::FinishNode,
    ];
    let parsed = parse_external_events(&EXTERNAL, source, &events).unwrap();
    assert_eq!(parsed.syntax().to_string(), source);
    assert_eq!(parsed.receipt().authority, "external-parser");
    assert_eq!(parsed.receipt().provider, "independent-parser");
    assert_eq!(parsed.receipt().provider_digest, EXTERNAL.provider_digest);
    assert_eq!(
        parsed.receipt().source_digest,
        "sha256:edd3a863872a04239eb29ad4bc12fc892b3d4ae57cc7e786a3697816f8e141c2"
    );
    assert_eq!(
        parsed.kind_name(gerbil_parser_runtime::SyntaxKind(1)),
        Some("Text")
    );
}

#[test]
fn invalid_provider_and_event_ranges_fail_with_source_bound_receipts() {
    let source = "é";
    let split = [
        TreeEvent::StartNode(0),
        TreeEvent::Token {
            kind: 1,
            start: 0,
            end: 1,
        },
        TreeEvent::FinishNode,
    ];
    let error = parse_external_events(&EXTERNAL, source, &split).unwrap_err();
    assert_eq!(error.diagnostic.reason_kind, "event-range");
    assert_eq!(error.receipt.authority, "external-parser");
    assert_eq!(
        error.receipt.source_digest,
        "sha256:4a99557e4033c3539de2eb65472017cad5f9557f7a0625a09f1c3f6e2ba69c4c"
    );

    let error = parse_external_events(&INVALID, source, &split).unwrap_err();
    assert_eq!(error.diagnostic.reason_kind, "invalid-external-provider");

    let rejection = ExternalParseError::from_provider(
        &EXTERNAL,
        source,
        Diagnostic {
            reason_kind: "provider-rejected",
            byte_offset: 0,
            message: "source is outside this parser's grammar".into(),
        },
    );
    assert_eq!(rejection.receipt.authority, "external-parser");
    assert_eq!(rejection.diagnostic.reason_kind, "provider-rejected");
}
