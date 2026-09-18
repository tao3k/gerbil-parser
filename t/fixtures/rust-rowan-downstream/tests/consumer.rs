use downstream_rowan_language_fixture::parse_records;

const ACCEPTED: &str = include_str!("../languages/records/v1/corpus/accepted.records");
const REJECTED: &str = include_str!("../languages/records/v1/corpus/rejected.records");

#[test]
fn custom_generated_language_is_lossless_and_structural() {
    let parsed = parse_records(ACCEPTED).expect("custom records grammar must accept its corpus");
    assert_eq!(parsed.syntax().to_string(), ACCEPTED);
    assert_eq!(parsed.receipt().language, "downstream-records");
    assert_eq!(parsed.receipt().version, "v1");
    assert_eq!(parsed.receipt().contract, "downstream-records.v1");
    assert!(parsed.receipt().grammar_digest.starts_with("sha256:"));

    let kinds = parsed
        .syntax()
        .descendants()
        .filter_map(|node| parsed.kind_name(node.kind()).map(str::to_owned))
        .collect::<Vec<_>>();
    assert_eq!(kinds.first().map(String::as_str), Some("Document"));
    assert_eq!(
        kinds
            .iter()
            .filter(|kind| kind.as_str() == "Assignment")
            .count(),
        3
    );
}

#[test]
fn custom_generated_language_rejects_incomplete_input_with_typed_evidence() {
    let error = parse_records(REJECTED).expect_err("incomplete assignment must reject");
    assert_eq!(error.receipt.language, "downstream-records");
    assert_eq!(error.receipt.contract, "downstream-records.v1");
    assert!(!error.diagnostic.reason_kind.is_empty());
    assert!(error.diagnostic.byte_offset <= REJECTED.len());
}
