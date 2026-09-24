//! The Rowan handoff for event functions generated from Scheme parser strategies.

use super::event_tree::build_rowan_events;
use super::model::{Diagnostic, LanguageSpec, Parse, ParseError, SelectiveGlrReceipt, TreeEvent};
use super::validation::{canonical_sha256_digest, receipt};

/// Validate Scheme-AOT parser events and publish the normal parse receipt.
///
/// This function does not recognize syntax or choose recovery behavior. Those
/// decisions belong to the generated event-producing function.
///
/// # Errors
///
/// Rejects an invalid parser identity or an event stream that is not one
/// balanced, source-exhaustive, UTF-8-safe tree for the supplied language.
pub fn parse_generated_events(
    language: &'static LanguageSpec,
    parser_digest: &'static str,
    source: &str,
    events: &[TreeEvent],
) -> Result<Parse, ParseError> {
    let parse_receipt = receipt(language, source, Some(parser_digest), None);
    let with_receipt = |diagnostic| ParseError {
        receipt: Box::new(parse_receipt.clone()),
        diagnostic: Box::new(diagnostic),
        selective_glr: None,
    };
    if !canonical_sha256_digest(parser_digest) {
        return Err(with_receipt(Diagnostic {
            reason_kind: "invalid-aot-artifact",
            byte_offset: 0,
            message: "generated parser digest is not canonical SHA-256".into(),
        }));
    }
    let green = build_rowan_events(language, source, events).map_err(with_receipt)?;
    Ok(Parse {
        green,
        kinds: language.kinds,
        receipt: parse_receipt,
        selective_glr: SelectiveGlrReceipt {
            branch_budget: 0,
            branches_explored: 0,
            speculative_branches_explored: 0,
            max_speculative_depth: 0,
            merged_branches: 0,
            successful_completions: 1,
            distinct_completions: 1,
            winner_reason: "scheme-aot-events",
            dynamic_score: 0,
        },
    })
}
