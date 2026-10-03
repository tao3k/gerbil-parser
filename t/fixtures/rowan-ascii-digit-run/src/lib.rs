//! An independent ASCII digit parser with a build-bound artifact and external receipt.

use gerbil_parser_rowan::{Diagnostic, EventCatalog, KindCategory, KindSpec, TreeEvent};
use gerbil_parser_rowan_external::{
    ExternalLanguageSpec, ExternalParse, ExternalParseError, parse_external_events,
};

include!(concat!(env!("OUT_DIR"), "/provider_digest.rs"));

const ARTIFACT: &[u8] = include_bytes!("parser.table");
static KINDS: &[KindSpec] = &[
    KindSpec {
        name: "Digits",
        category: KindCategory::Node,
    },
    KindSpec {
        name: "DigitRun",
        category: KindCategory::Token,
    },
];
static LANGUAGE: ExternalLanguageSpec = ExternalLanguageSpec {
    language: "ascii-digit-run",
    version: "fixture",
    contract: "ascii-digit-run.syntax",
    provider: "ascii-digit-table",
    provider_version: "fixture",
    provider_digest: PROVIDER_DIGEST,
    catalog: EventCatalog {
        root_kind: 0,
        kinds: KINDS,
    },
};

/// Parse an ASCII digit run using the build-bound provider table.
///
/// # Errors
///
/// Rejects source outside the digit grammar or invalid source events.
pub fn parse_digits(source: &str) -> Result<ExternalParse, ExternalParseError> {
    // The fixture interprets the artifact's declared character range itself.
    let parser_range = ARTIFACT
        .strip_prefix(b"digit-token:ascii:")
        .and_then(|bytes| bytes.strip_suffix(b"\n"));
    if parser_range != Some(&b"0-9"[..])
        || source.is_empty()
        || !source.bytes().all(|byte| byte.is_ascii_digit())
    {
        return Err(ExternalParseError::from_provider(
            &LANGUAGE,
            source,
            Diagnostic {
                reason_kind: "provider-rejected",
                byte_offset: 0,
                message: "expected an ASCII digit run".into(),
            },
        ));
    }
    parse_external_events(
        &LANGUAGE,
        source,
        &[
            TreeEvent::StartNode(0),
            TreeEvent::Token {
                kind: 1,
                start: 0,
                end: source.len(),
            },
            TreeEvent::FinishNode,
        ],
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn independent_parser_emits_lossless_external_tree_and_bound_receipt() {
        let parsed = parse_digits("0123").unwrap();
        assert_eq!(parsed.syntax().to_string(), "0123");
        assert_eq!(parsed.receipt().authority, "external-parser");
        assert_eq!(parsed.receipt().provider_digest, PROVIDER_DIGEST);
        assert_eq!(parsed.receipt().language, "ascii-digit-run");
        assert_eq!(
            parse_digits("12x").unwrap_err().diagnostic.reason_kind,
            "provider-rejected"
        );
    }
}
