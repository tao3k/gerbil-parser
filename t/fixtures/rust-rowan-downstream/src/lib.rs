//! Independent downstream consumer of one custom generated language module.

#[path = "generated/records_v1.rs"]
pub mod records_v1;

/// Parse source with the downstream-owned records grammar.
///
/// # Errors
///
/// Returns the generic engine diagnostic when the generated grammar rejects
/// the source.
pub fn parse_records(
    source: &str,
) -> Result<gerbil_parser_rowan::Parse, gerbil_parser_rowan::ParseError> {
    gerbil_parser_rowan::parse(&records_v1::LANGUAGE, source)
}
