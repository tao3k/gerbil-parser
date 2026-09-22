//! ISO GQL language parsing entrypoint.

use crate::LANGUAGE;

/// Parse GQL source with the generated language product.
///
/// # Errors
///
/// Returns a typed parser error when the source is rejected or the generated
/// language tables violate the runtime contract.
pub fn parse(source: &str) -> Result<gerbil_parser_rowan::Parse, gerbil_parser_rowan::ParseError> {
    gerbil_parser_rowan::parse(&LANGUAGE, source)
}
