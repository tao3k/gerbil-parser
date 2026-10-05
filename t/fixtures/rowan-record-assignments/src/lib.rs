//! Independent consumer of the generated record assignments language module.

#[path = "generated/records.rs"]
pub mod records;

/// Parse source with the record assignments grammar.
///
/// # Errors
///
/// Returns the generic engine diagnostic when the generated grammar rejects
/// the source.
pub fn parse_records(
    source: &str,
) -> Result<gerbil_parser_rowan::Parse, gerbil_parser_rowan::ParseError> {
    gerbil_parser_rowan::parse(&records::LANGUAGE, source)
}
