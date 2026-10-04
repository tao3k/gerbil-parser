//! Complete descriptor + shared scanner AOT, with no language callbacks.
use crate::records_contextual_fixture::generated;
use gerbil_parser_rowan::{parse, parse_contextual};
#[test]
fn contextual_lr_matches_canonical_lossless_tree() {
    for source in [
        "",
        "α=1\n",
        "a = 1\r\nb=\"β\"\n",
        "name=value",
        "a=\"x\"\"y\"\n",
    ] {
        let expected = parse(&generated::LANGUAGE, source).expect("canonical");
        let actual = parse_contextual(&generated::CONTEXTUAL, source).expect("contextual");
        assert_eq!(
            format!("{:#?}", actual.syntax()),
            format!("{:#?}", expected.syntax())
        );
        assert_eq!(actual.syntax().to_string(), source);
        assert_eq!(
            actual.receipt().source_digest,
            expected.receipt().source_digest
        );
        assert_eq!(
            actual.receipt().parser_digest,
            Some(generated::CONTEXTUAL.parser_digest)
        );
        assert_eq!(
            actual.receipt().scanner_digest,
            Some(generated::CONTEXTUAL.scanner.digest)
        );
    }
}
#[test]
fn contextual_rejects_incomplete_source() {
    for source in ["a=", "a=\"unclosed", "=1", "a=2"] {
        assert!(parse_contextual(&generated::CONTEXTUAL, source).is_err());
    }
}

#[test]
fn scanner_from_another_grammar_is_rejected_before_execution() {
    use gerbil_parser_rowan::{ContextualParserSpec, scanner::ScannerSpec};
    static OTHER: ScannerSpec = ScannerSpec {
        base_grammar_digest: None,
        ..generated::contextual_scanner::SCANNER
    };
    static PRODUCT: ContextualParserSpec = ContextualParserSpec {
        scanner: &OTHER,
        ..generated::CONTEXTUAL
    };
    assert!(parse_contextual(&PRODUCT, "a=1").is_err());
}

#[test]
fn shared_product_catalog_preserves_parallel_receipts_after_rejection() {
    std::thread::scope(|scope| {
        let workers: Vec<_> = ["α=1\n", "a = 1\r\nb=\"β\"\n", "name=value", ""]
            .into_iter()
            .map(|source| {
                scope.spawn(move || {
                    for _ in 0..8 {
                        assert!(parse_contextual(&generated::CONTEXTUAL, "a=").is_err());
                        let expected = parse(&generated::LANGUAGE, source).expect("canonical");
                        let actual =
                            parse_contextual(&generated::CONTEXTUAL, source).expect("contextual");
                        assert_eq!(actual.syntax().to_string(), source);
                        assert_eq!(
                            actual.receipt().source_digest,
                            expected.receipt().source_digest
                        );
                    }
                })
            })
            .collect();
        for worker in workers {
            worker.join().expect("source-local receipt");
        }
    });
}
