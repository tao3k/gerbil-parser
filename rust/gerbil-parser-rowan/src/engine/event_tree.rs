//! Validated Rowan sink for structure emitted by Scheme-AOT parsers.

use rowan::{GreenNode, GreenToken, NodeOrToken};

use super::model::{Diagnostic, KindCategory, LanguageSpec, TreeEvent};
use super::validation::validate_spec_once;

/// Build one lossless Rowan tree from AOT parser events.
///
/// The generic engine checks generated kind identities, balanced node events,
/// and ordered UTF-8 token coverage. Language-specific structure and recovery
/// decisions belong to the downstream Scheme language pack.
///
/// # Errors
///
/// Returns a typed diagnostic if the generated specification or event stream
/// cannot form one root covering the exact source.
pub fn build_rowan_events(
    spec: &'static LanguageSpec,
    source: &str,
    events: &[TreeEvent],
) -> Result<GreenNode, Diagnostic> {
    validate_spec_once(spec).map_err(|message| diagnostic("invalid-aot-artifact", 0, message))?;
    let mut frames: Vec<(u16, Vec<NodeOrToken<GreenNode, GreenToken>>)> = Vec::new();
    let mut root = None;
    let mut offset = 0usize;

    for event in events {
        match *event {
            TreeEvent::StartNode(kind) => {
                if root.is_some() || (frames.is_empty() && kind != spec.root_kind) {
                    return Err(diagnostic(
                        "event-root",
                        offset,
                        "events must contain exactly one declared root node",
                    ));
                }
                validate_kind(spec, kind, KindCategory::Node, offset)?;
                frames.push((kind, Vec::new()));
            }
            TreeEvent::Token { kind, start, end } => {
                let Some((_, children)) = frames.last_mut() else {
                    return Err(diagnostic(
                        "event-nesting",
                        start,
                        "a token must be inside the root node",
                    ));
                };
                validate_kind(spec, kind, KindCategory::Token, start)?;
                if start != offset
                    || end <= start
                    || end > source.len()
                    || !source.is_char_boundary(start)
                    || !source.is_char_boundary(end)
                {
                    return Err(diagnostic(
                        "event-range",
                        start.min(source.len()),
                        "tokens must cover ordered, nonempty UTF-8 source ranges",
                    ));
                }
                children.push(GreenToken::new(rowan::SyntaxKind(kind), &source[start..end]).into());
                offset = end;
            }
            TreeEvent::FinishNode => {
                let Some((kind, children)) = frames.pop() else {
                    return Err(diagnostic(
                        "event-nesting",
                        offset,
                        "node finish has no matching start",
                    ));
                };
                let node = GreenNode::new(rowan::SyntaxKind(kind), children);
                if let Some((_, parent)) = frames.last_mut() {
                    parent.push(node.into());
                } else {
                    root = Some(node);
                }
            }
        }
    }

    let Some(green) = root else {
        return Err(diagnostic(
            "event-nesting",
            offset,
            "event stream does not close one root node",
        ));
    };
    if !frames.is_empty() {
        return Err(diagnostic(
            "event-nesting",
            offset,
            "event stream leaves a node open",
        ));
    }
    if offset != source.len() {
        return Err(diagnostic(
            "event-range",
            offset,
            "event stream does not cover the source suffix",
        ));
    }
    if usize::from(green.text_len()) != source.len() {
        return Err(diagnostic(
            "event-range",
            offset,
            "Rowan tree does not cover the complete source",
        ));
    }
    Ok(green)
}

fn validate_kind(
    spec: &LanguageSpec,
    kind: u16,
    category: KindCategory,
    offset: usize,
) -> Result<(), Diagnostic> {
    if spec
        .kinds
        .get(usize::from(kind))
        .is_none_or(|entry| entry.category != category)
    {
        return Err(diagnostic(
            "event-kind",
            offset,
            "event references an unknown or incorrectly categorized syntax kind",
        ));
    }
    Ok(())
}

fn diagnostic(
    reason_kind: &'static str,
    byte_offset: usize,
    message: impl Into<String>,
) -> Diagnostic {
    Diagnostic {
        reason_kind,
        byte_offset,
        message: message.into(),
    }
}
