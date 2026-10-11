//! Navigation indexes point into original GPA1 records and source; no CST copy.
use crate::{NativeArtifactError, NativeCatalog, NativeEvent, NativeEventKind as Kind};
use crate::{admission, wire};
use std::borrow::Cow;

/// UTF-8 byte span in the source admitted with this result.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SourceRange {
    pub start: usize,
    pub end: usize,
}
#[derive(Clone, Copy, Debug)]
pub(crate) struct Index {
    end: usize,
    parent: Option<usize>,
}
/// Validated native bytes and source borrowed from the same parse result.
#[derive(Debug)]
pub struct NativeArtifactView<'a> {
    payload: &'a [u8],
    source: &'a str,
    catalog: &'a NativeCatalog,
    accepted: bool,
    index: Cow<'a, [Index]>,
}
impl<'a> NativeArtifactView<'a> {
    /// Validate identities, vocabulary, event nesting and full UTF-8 coverage.
    /// # Errors
    /// Rejects malformed or mismatched artifacts before exposing navigation.
    pub fn decode(
        payload: &'a [u8],
        source: &'a str,
        catalog: &'a NativeCatalog,
    ) -> Result<Self, NativeArtifactError> {
        let navigation = Navigation::decode(payload, source, catalog)?;
        Ok(Self {
            payload,
            source,
            catalog,
            accepted: navigation.accepted,
            index: Cow::Owned(navigation.index),
        })
    }
    pub(crate) fn from_navigation(
        payload: &'a [u8],
        source: &'a str,
        catalog: &'a NativeCatalog,
        navigation: &'a Navigation,
    ) -> Self {
        Self {
            payload,
            source,
            catalog,
            accepted: navigation.accepted,
            index: Cow::Borrowed(&navigation.index),
        }
    }
    /// Whether Scheme accepted this source without repair.
    #[must_use]
    pub fn accepted(&self) -> bool {
        self.accepted
    }
    /// Catalog from the same native language handle.
    #[must_use]
    pub fn catalog(&self) -> &NativeCatalog {
        self.catalog
    }
    /// Borrow wire records without copying payloads or token text.
    pub fn events(&self) -> impl ExactSizeIterator<Item = NativeEvent> + '_ {
        self.payload[80..]
            .as_chunks::<24>()
            .0
            .iter()
            .map(wire::event)
    }
    /// Borrow all scanner tokens in producer order, including syntax rejection.
    pub fn tokens(&self) -> impl Iterator<Item = NativeToken<'_, 'a>> + '_ {
        (0..self.index.len()).filter_map(|at| {
            (self.event(at).kind == Kind::Token).then_some(NativeToken { view: self, at })
        })
    }
    /// Return a token source slice. Callers cannot forge an out-of-source range.
    #[must_use]
    pub fn token_text(&self, event: NativeEvent) -> Option<&'a str> {
        (event.kind == Kind::Token)
            .then(|| self.source.get(event.start..event.end))
            .flatten()
    }
    fn event(&self, at: usize) -> NativeEvent {
        wire::event(
            self.payload[80 + at * 24..80 + (at + 1) * 24]
                .try_into()
                .expect("admitted width"),
        )
    }
    /// Borrow the accepted root; rejected input has tokens but no accepted AST.
    #[must_use]
    pub fn root(&self) -> Option<NativeNode<'_, 'a>> {
        self.accepted.then_some(NativeNode { view: self, at: 0 })
    }
    fn range(&self, at: usize) -> SourceRange {
        SourceRange {
            start: self.event(at).start,
            end: self.event(self.index[at].end).end,
        }
    }
}
/// Borrowed AST node; ids and fields remain those emitted by Scheme.
#[derive(Clone, Copy, Debug)]
pub struct NativeNode<'v, 'a> {
    view: &'v NativeArtifactView<'a>,
    at: usize,
}
impl<'v, 'a> NativeNode<'v, 'a> {
    /// Canonical producer node id.
    #[must_use]
    pub fn id(&self) -> u64 {
        self.view.event(self.at).id
    }
    /// Published node kind name.
    #[must_use]
    pub fn kind(&self) -> &'a str {
        &self.view.catalog.kinds[self.view.event(self.at).symbol as usize].name
    }
    /// Exact source range of this node.
    #[must_use]
    pub fn range(&self) -> SourceRange {
        self.view.range(self.at)
    }
    /// Borrow the original source substring, including trivia.
    #[must_use]
    pub fn text(&self) -> &'a str {
        let r = self.range();
        &self.view.source[r.start..r.end]
    }
    /// Parent node, treating field scopes as associations rather than nodes.
    #[must_use]
    pub fn parent(&self) -> Option<Self> {
        self.view.index[self.at].parent.map(|at| Self {
            view: self.view,
            at,
        })
    }
    /// Direct node/token children, preserving order across field scopes.
    #[must_use]
    pub fn children(&self) -> NativeChildren<'v, 'a> {
        NativeChildren {
            view: self.view,
            next: self.at + 1,
            end: self.view.index[self.at].end,
        }
    }
    /// Direct named field scopes; repeated fields remain distinct.
    #[must_use]
    pub fn fields(&self) -> NativeFields<'v, 'a> {
        NativeFields {
            view: self.view,
            next: self.at + 1,
            end: self.view.index[self.at].end,
        }
    }
    /// All occurrences of a declared field in source order.
    pub fn field(&self, name: &str) -> impl Iterator<Item = NativeField<'v, 'a>> {
        self.fields().filter(move |f| f.name() == name)
    }
}
/// Borrowed field scope, including an empty or repeated field value.
#[derive(Clone, Copy, Debug)]
pub struct NativeField<'v, 'a> {
    view: &'v NativeArtifactView<'a>,
    at: usize,
}
impl<'v, 'a> NativeField<'v, 'a> {
    /// Published field name.
    #[must_use]
    pub fn name(&self) -> &'a str {
        &self.view.catalog.fields[self.view.event(self.at).symbol as usize]
    }
    /// Exact source span of this field.
    #[must_use]
    pub fn range(&self) -> SourceRange {
        self.view.range(self.at)
    }
    /// Source text of this field, without copying.
    #[must_use]
    pub fn text(&self) -> &'a str {
        let r = self.range();
        &self.view.source[r.start..r.end]
    }
    /// Direct field values in producer order.
    #[must_use]
    pub fn children(&self) -> NativeChildren<'v, 'a> {
        NativeChildren {
            view: self.view,
            next: self.at + 1,
            end: self.view.index[self.at].end,
        }
    }
}
/// Borrowed source token from the native scanner's terminal catalog.
#[derive(Clone, Copy, Debug)]
pub struct NativeToken<'v, 'a> {
    view: &'v NativeArtifactView<'a>,
    at: usize,
}
impl<'v, 'a> NativeToken<'v, 'a> {
    /// Scanner token ordinal, independent of syntax node ids.
    #[must_use]
    pub fn id(&self) -> u64 {
        self.view.event(self.at).id
    }
    /// Original scanner class, before its mapping to a syntax token kind.
    #[must_use]
    pub fn class(&self) -> &'a str {
        &self.view.catalog.terminals[self.view.event(self.at).symbol as usize].name
    }

    /// Published syntax kind, falling back to the scanner class when unmapped.
    #[must_use]
    pub fn kind(&self) -> &'a str {
        let terminal = &self.view.catalog.terminals[self.view.event(self.at).symbol as usize];
        terminal.syntax_kind.map_or(terminal.name.as_str(), |kind| {
            self.view.catalog.kinds[kind].name.as_str()
        })
    }
    /// Exact UTF-8 byte span.
    #[must_use]
    pub fn range(&self) -> SourceRange {
        self.view.range(self.at)
    }
    /// Borrow the original token text.
    #[must_use]
    pub fn text(&self) -> &'a str {
        let r = self.range();
        &self.view.source[r.start..r.end]
    }
    /// Owning node, if this is accepted syntax.
    #[must_use]
    pub fn parent(&self) -> Option<NativeNode<'v, 'a>> {
        self.view.index[self.at].parent.map(|at| NativeNode {
            view: self.view,
            at,
        })
    }
}
/// A direct native AST child.
#[derive(Clone, Copy, Debug)]
pub enum NativeElement<'v, 'a> {
    Node(NativeNode<'v, 'a>),
    Token(NativeToken<'v, 'a>),
}
/// Iterator over direct children, skipping each child subtree in constant time.
pub struct NativeChildren<'v, 'a> {
    view: &'v NativeArtifactView<'a>,
    next: usize,
    end: usize,
}
impl<'v, 'a> Iterator for NativeChildren<'v, 'a> {
    type Item = NativeElement<'v, 'a>;
    fn next(&mut self) -> Option<Self::Item> {
        while self.next < self.end {
            let at = self.next;
            self.next += 1;
            match self.view.event(at).kind {
                Kind::StartNode => {
                    self.next = self.view.index[at].end + 1;
                    return Some(NativeElement::Node(NativeNode {
                        view: self.view,
                        at,
                    }));
                }
                Kind::Token => {
                    return Some(NativeElement::Token(NativeToken {
                        view: self.view,
                        at,
                    }));
                }
                _ => {}
            }
        }
        None
    }
}
/// Iterator over direct fields, preserving repeated and empty scopes.
pub struct NativeFields<'v, 'a> {
    view: &'v NativeArtifactView<'a>,
    next: usize,
    end: usize,
}
impl<'v, 'a> Iterator for NativeFields<'v, 'a> {
    type Item = NativeField<'v, 'a>;
    fn next(&mut self) -> Option<Self::Item> {
        while self.next < self.end {
            let at = self.next;
            self.next += 1;
            match self.view.event(at).kind {
                Kind::StartField => {
                    self.next = self.view.index[at].end + 1;
                    return Some(NativeField {
                        view: self.view,
                        at,
                    });
                }
                Kind::StartNode => self.next = self.view.index[at].end + 1,
                _ => {}
            }
        }
        None
    }
}

/// Admission and navigation contain no references into their payload owner.
#[derive(Debug)]
pub(crate) struct Navigation {
    accepted: bool,
    index: Vec<Index>,
}
impl Navigation {
    pub(crate) fn decode(
        payload: &[u8],
        source: &str,
        catalog: &NativeCatalog,
    ) -> Result<Self, NativeArtifactError> {
        let accepted = admission::admit(payload, source, catalog)?;
        let count = (payload.len() - 80) / 24;
        let mut index = vec![
            Index {
                end: 0,
                parent: None
            };
            count
        ];
        let mut stack = Vec::new();
        let mut nodes = Vec::new();
        for (at, row) in payload[80..].as_chunks::<24>().0.iter().enumerate() {
            let event = wire::event(row);
            index[at].parent = nodes.last().copied();
            match event.kind {
                Kind::StartNode | Kind::StartField => {
                    stack.push(at);
                    if event.kind == Kind::StartNode {
                        nodes.push(at);
                    }
                }
                Kind::FinishNode | Kind::FinishField => {
                    let start = stack
                        .pop()
                        .ok_or_else(|| wire::error("index-nesting", Some(at)))?;
                    index[start].end = at;
                    if event.kind == Kind::FinishNode {
                        nodes.pop();
                    }
                }
                Kind::Token => {
                    index[at].end = at;
                }
            }
        }
        Ok(Self { accepted, index })
    }
}

#[cfg(test)]
#[path = "../tests/unit/storage.rs"]
mod storage_tests;
