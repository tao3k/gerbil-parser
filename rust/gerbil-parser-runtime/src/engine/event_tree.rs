//! Validated indexed syntax sink for source-backed structure events.

use gerbil_parser_artifact::syntax::{IndexedEntry, SyntaxTree};

use super::model::{Diagnostic, EventCatalog, KindCategory, LanguageSpec, TreeEvent};
use super::validation::validate_spec_once;

/// Build one lossless indexed syntax tree from AOT parser events.
///
/// The generic engine checks generated kind identities, balanced node events,
/// and ordered UTF-8 token coverage. Language-specific structure and recovery
/// decisions belong to the downstream Scheme language pack.
///
/// # Errors
///
/// Returns a typed diagnostic if the generated specification or event stream
/// cannot form one root covering the exact source.
pub fn build_syntax_events(
    spec: &'static LanguageSpec,
    source: &str,
    events: &[TreeEvent],
) -> Result<SyntaxTree, Diagnostic> {
    validate_spec_once(spec).map_err(|message| diagnostic("invalid-aot-artifact", 0, message))?;
    build_syntax_events_catalog(
        &EventCatalog {
            root_kind: spec.root_kind,
            kinds: spec.kinds,
        },
        source,
        events,
    )
}

/// Build one lossless tree from an independently authored event producer.
/// This validates structure and source coverage, not the producer's grammar
/// or its authority receipt.
///
/// # Errors
///
/// Rejects an invalid kind catalog or event stream.
pub fn build_syntax_events_catalog(
    catalog: &EventCatalog,
    source: &str,
    events: &[TreeEvent],
) -> Result<SyntaxTree, Diagnostic> {
    build_syntax_event_iter(catalog, source, events.iter().copied())
}

pub(crate) fn build_syntax_event_iter(
    catalog: &EventCatalog,
    source: &str,
    events: impl IntoIterator<Item = TreeEvent>,
) -> Result<SyntaxTree, Diagnostic> {
    validate_catalog(catalog)?;
    let mut frames: Vec<(u16, usize)> = Vec::new();
    let mut entries: Vec<IndexedEntry> = Vec::new();
    let mut root = None;
    let mut offset = 0usize;

    for event in events {
        match event {
            TreeEvent::StartNode(kind) => {
                if root.is_some() || (frames.is_empty() && kind != catalog.root_kind) {
                    return Err(diagnostic(
                        "event-root",
                        offset,
                        "events must contain exactly one declared root node",
                    ));
                }
                validate_kind(catalog, kind, KindCategory::Node, offset)?;
                let at = entries.len();
                entries.push(IndexedEntry {
                    kind,
                    token: false,
                    start: offset,
                    end: 0,
                    parent: frames.last().map(|f| f.1),
                    subtree_end: 0,
                });
                frames.push((kind, at));
            }
            TreeEvent::Token { kind, start, end } => {
                let Some((_, parent)) = frames.last() else {
                    return Err(diagnostic(
                        "event-nesting",
                        start,
                        "a token must be inside the root node",
                    ));
                };
                validate_kind(catalog, kind, KindCategory::Token, start)?;
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
                entries.push(IndexedEntry {
                    kind,
                    token: true,
                    start,
                    end,
                    parent: Some(*parent),
                    subtree_end: entries.len() + 1,
                });
                offset = end;
            }
            TreeEvent::FinishNode => {
                let Some((_kind, at)) = frames.pop() else {
                    return Err(diagnostic(
                        "event-nesting",
                        offset,
                        "node finish has no matching start",
                    ));
                };
                entries[at].end = offset;
                entries[at].subtree_end = entries.len();
                if frames.is_empty() {
                    root = Some(at);
                }
            }
        }
    }

    let Some(_root) = root else {
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
    SyntaxTree::from_index(source, entries)
        .map_err(|message| diagnostic("event-range", offset, message))
}

fn validate_catalog(catalog: &EventCatalog) -> Result<(), Diagnostic> {
    if catalog.kinds.len() > usize::from(u16::MAX) + 1
        || catalog
            .kinds
            .get(usize::from(catalog.root_kind))
            .is_none_or(|root| root.category != KindCategory::Node)
    {
        return Err(diagnostic(
            "invalid-event-catalog",
            0,
            "event catalog requires a declared node root and bounded kinds",
        ));
    }
    Ok(())
}

fn validate_kind(
    catalog: &EventCatalog,
    kind: u16,
    category: KindCategory,
    offset: usize,
) -> Result<(), Diagnostic> {
    if catalog
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
