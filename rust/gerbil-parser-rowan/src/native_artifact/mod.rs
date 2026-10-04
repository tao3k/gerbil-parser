//! Borrowed GPA1 transport views for compiled Scheme language packs.
mod admission;
mod wire;

use crate::{EventCatalog, LanguageSpec, TreeEvent};
use rowan::GreenNode;
pub use wire::{NativeArtifactError, NativeEvent, NativeEventKind};

/// Validated native bytes bound to their source and generated language catalog.
/// The C payload must remain alive while this view or its iterators are used.
#[derive(Debug)]
pub struct NativeArtifactView<'a> {
    payload: &'a [u8],
    source: &'a str,
    language: &'static LanguageSpec,
    accepted: bool,
}

impl<'a> NativeArtifactView<'a> {
    /// Admit a GPA1 result using the field count from the same native descriptor.
    /// Transport status must have been checked before passing the payload.
    /// # Errors
    /// Rejects incompatible headers, identities, catalogs, or event streams.
    pub fn decode(
        payload: &'a [u8],
        source: &'a str,
        language: &'static LanguageSpec,
        field_count: usize,
    ) -> Result<Self, NativeArtifactError> {
        let accepted = admission::admit(payload, source, language, field_count)?;
        Ok(Self {
            payload,
            source,
            language,
            accepted,
        })
    }

    #[must_use]
    pub fn accepted(&self) -> bool {
        self.accepted
    }

    /// Iterate borrowed wire records without copying the payload or lexemes.
    pub fn events(&self) -> impl ExactSizeIterator<Item = NativeEvent> + '_ {
        self.payload[80..].chunks_exact(24).map(wire::event)
    }

    /// Retrieve the original source slice for a token record.
    #[must_use]
    pub fn token_text(&self, event: NativeEvent) -> Option<&'a str> {
        (event.kind == NativeEventKind::Token)
            .then(|| self.source.get(event.start..event.end))
            .flatten()
    }

    /// Build Rowan directly from the validated iterator, without a second event vector.
    /// Field records remain accessible through `events`; Rowan stores node/token structure.
    /// # Errors
    /// Syntax rejection never publishes an accepted tree. Invalid catalogs also fail.
    pub fn to_rowan(&self) -> Result<GreenNode, NativeArtifactError> {
        if !self.accepted {
            return Err(wire::error("rejected-syntax", None));
        }
        let events = self.events().filter_map(|event| match event.kind {
            NativeEventKind::StartNode => {
                Some(TreeEvent::StartNode(wire::kind_index(event.symbol)))
            }
            NativeEventKind::FinishNode => Some(TreeEvent::FinishNode),
            NativeEventKind::Token => Some(TreeEvent::Token {
                kind: self.language.terminals[event.symbol as usize].syntax_kind,
                start: event.start,
                end: event.end,
            }),
            NativeEventKind::StartField | NativeEventKind::FinishField => None,
        });
        crate::engine::event_tree::build_rowan_event_iter(
            &EventCatalog {
                root_kind: self.language.root_kind,
                kinds: self.language.kinds,
            },
            self.source,
            events,
        )
        .map_err(|diagnostic| wire::error(diagnostic.reason_kind, None))
    }
}

#[cfg(test)]
#[path = "../../tests/unit/native_artifact.rs"]
mod tests;
