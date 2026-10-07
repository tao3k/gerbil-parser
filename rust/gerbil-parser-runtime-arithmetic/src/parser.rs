//! Arithmetic language parsing entrypoint.

use crate::LANGUAGE;

/// Parse arithmetic source with the generated language product.
///
/// # Errors
///
/// Returns a typed parser error when the source is rejected or the generated
/// language tables violate the runtime contract.
pub fn parse(
    source: &str,
) -> Result<gerbil_parser_runtime::Parse, gerbil_parser_runtime::ParseError> {
    gerbil_parser_runtime::parse(&LANGUAGE, source)
}
