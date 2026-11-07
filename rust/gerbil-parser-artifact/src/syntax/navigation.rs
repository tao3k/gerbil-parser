//! Node and token access over an existing flat source index.
use super::storage::{Storage, SyntaxKind, TextRange, TextSize};
use super::walk::WalkEvent;
use std::{fmt, sync::Arc};
/// An owned-source node view, sharing the producer's navigation index.
#[derive(Clone)]
pub struct SyntaxNode {
    pub(super) storage: Arc<Storage>,
    pub(super) at: usize,
}
/// An owned-source token view, borrowing its text from the shared source.
#[derive(Clone)]
pub struct SyntaxToken {
    pub(super) storage: Arc<Storage>,
    pub(super) at: usize,
}
/// Node or token child in producer order.
#[derive(Clone, Debug)]
pub enum SyntaxElement {
    Node(SyntaxNode),
    Token(SyntaxToken),
}
impl SyntaxElement {
    /// Keep a token child, if this element is a token.
    #[must_use]
    pub fn into_token(self) -> Option<SyntaxToken> {
        match self {
            Self::Token(t) => Some(t),
            Self::Node(_) => None,
        }
    }
    /// Keep a node child, if this element is a node.
    #[must_use]
    pub fn into_node(self) -> Option<SyntaxNode> {
        match self {
            Self::Node(n) => Some(n),
            Self::Token(_) => None,
        }
    }
    /// Shared published kind.
    #[must_use]
    pub fn kind(&self) -> SyntaxKind {
        match self {
            Self::Node(n) => n.kind(),
            Self::Token(t) => t.kind(),
        }
    }
    /// Shared original source range.
    #[must_use]
    // Construction rejects sources beyond u32 and spans beyond source length.
    #[allow(clippy::cast_possible_truncation)]
    pub fn text_range(&self) -> TextRange {
        match self {
            Self::Node(n) => n.text_range(),
            Self::Token(t) => t.text_range(),
        }
    }
}
fn element(storage: &Arc<Storage>, at: usize) -> SyntaxElement {
    if storage.entries[at].token {
        SyntaxElement::Token(SyntaxToken {
            storage: Arc::clone(storage),
            at,
        })
    } else {
        SyntaxElement::Node(SyntaxNode {
            storage: Arc::clone(storage),
            at,
        })
    }
}
impl SyntaxNode {
    /// Published node kind.
    #[must_use]
    pub fn kind(&self) -> SyntaxKind {
        SyntaxKind(self.storage.entries[self.at].kind)
    }
    /// Original source span.
    #[must_use]
    // Construction rejects sources beyond u32 and spans beyond source length.
    #[allow(clippy::cast_possible_truncation)]
    pub fn text_range(&self) -> TextRange {
        let r = &self.storage.entries[self.at];
        TextRange::new(TextSize(r.start as u32), TextSize(r.end as u32))
    }
    /// Borrow the source slice directly, including trivia.
    #[must_use]
    pub fn text(&self) -> &str {
        let r = &self.storage.entries[self.at];
        &self.storage.source[r.start..r.end]
    }
    /// Existing parent index.
    #[must_use]
    pub fn parent(&self) -> Option<Self> {
        self.storage.entries[self.at].parent.map(|at| Self {
            storage: Arc::clone(&self.storage),
            at,
        })
    }
    /// Direct children, skipping each subtree using its admitted interval.
    pub fn children_with_tokens(&self) -> impl Iterator<Item = SyntaxElement> + use<> {
        let storage = Arc::clone(&self.storage);
        let end = storage.entries[self.at].subtree_end;
        let mut next = self.at + 1;
        std::iter::from_fn(move || {
            if next >= end {
                return None;
            }
            let at = next;
            next = storage.entries[at].subtree_end;
            Some(element(&storage, at))
        })
    }
    /// Direct node children.
    pub fn children(&self) -> impl Iterator<Item = Self> + use<> {
        self.children_with_tokens()
            .filter_map(SyntaxElement::into_node)
    }
    /// First direct node child.
    #[must_use]
    pub fn first_child(&self) -> Option<Self> {
        self.children().next()
    }
    /// Preorder nodes including this node, without recursively traversing Rust stacks.
    pub fn descendants(&self) -> impl Iterator<Item = Self> + use<> {
        self.descendants_with_tokens()
            .filter_map(SyntaxElement::into_node)
    }
    /// Preorder elements including this node.
    pub fn descendants_with_tokens(&self) -> impl Iterator<Item = SyntaxElement> + use<> {
        let storage = Arc::clone(&self.storage);
        let range = self.at..storage.entries[self.at].subtree_end;
        range.map(move |at| element(&storage, at))
    }
    /// Enter/leave traversal over existing indexes.
    pub fn preorder_with_tokens(&self) -> impl Iterator<Item = WalkEvent<SyntaxElement>> + use<> {
        Preorder::new(Arc::clone(&self.storage), self.at)
    }
}
impl SyntaxToken {
    /// Published token kind.
    #[must_use]
    pub fn kind(&self) -> SyntaxKind {
        SyntaxKind(self.storage.entries[self.at].kind)
    }
    /// Exact source slice, without allocating token strings.
    #[must_use]
    pub fn text(&self) -> &str {
        let r = &self.storage.entries[self.at];
        &self.storage.source[r.start..r.end]
    }
    /// Original byte range.
    #[must_use]
    // Construction rejects sources beyond u32 and spans beyond source length.
    #[allow(clippy::cast_possible_truncation)]
    pub fn text_range(&self) -> TextRange {
        let r = &self.storage.entries[self.at];
        TextRange::new(TextSize(r.start as u32), TextSize(r.end as u32))
    }
    /// Existing owning node.
    #[must_use]
    pub fn parent(&self) -> Option<SyntaxNode> {
        self.storage.entries[self.at].parent.map(|at| SyntaxNode {
            storage: Arc::clone(&self.storage),
            at,
        })
    }
}
impl fmt::Display for SyntaxNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.text())
    }
}
impl fmt::Display for SyntaxToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.text())
    }
}
impl fmt::Debug for SyntaxNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SyntaxNode")
            .field("kind", &self.kind())
            .field("range", &self.text_range())
            .field(
                "entries",
                &&self.storage.entries[self.at..self.storage.entries[self.at].subtree_end],
            )
            .finish()
    }
}
impl fmt::Debug for SyntaxToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SyntaxToken")
            .field("kind", &self.kind())
            .field("range", &self.text_range())
            .field("text", &self.text())
            .finish()
    }
}
impl PartialEq for SyntaxNode {
    fn eq(&self, other: &Self) -> bool {
        self.at == other.at && Arc::ptr_eq(&self.storage, &other.storage)
    }
}
impl Eq for SyntaxNode {}
pub(super) struct Preorder {
    storage: Arc<Storage>,
    next: usize,
    end: usize,
    stack: Vec<usize>,
    pending_token: Option<usize>,
}
impl Preorder {
    pub(super) fn new(storage: Arc<Storage>, at: usize) -> Self {
        let end = storage.entries[at].subtree_end;
        Self {
            storage,
            next: at,
            end,
            stack: Vec::new(),
            pending_token: None,
        }
    }
}
impl Iterator for Preorder {
    type Item = WalkEvent<SyntaxElement>;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(at) = self.pending_token.take() {
            return Some(WalkEvent::Leave(SyntaxElement::Token(SyntaxToken {
                storage: Arc::clone(&self.storage),
                at,
            })));
        }
        if let Some(&at) = self.stack.last()
            && self.next == self.storage.entries[at].subtree_end
        {
            self.stack.pop();
            return Some(WalkEvent::Leave(SyntaxElement::Node(SyntaxNode {
                storage: Arc::clone(&self.storage),
                at,
            })));
        }
        if self.next >= self.end {
            return None;
        }
        let at = self.next;
        self.next += 1;
        let value = if self.storage.entries[at].token {
            self.pending_token = Some(at);
            SyntaxElement::Token(SyntaxToken {
                storage: Arc::clone(&self.storage),
                at,
            })
        } else {
            self.stack.push(at);
            SyntaxElement::Node(SyntaxNode {
                storage: Arc::clone(&self.storage),
                at,
            })
        };
        Some(WalkEvent::Enter(value))
    }
}

impl super::storage::SyntaxTree {
    /// Access the existing root index; no additional tree is built.
    #[must_use]
    pub fn root(&self) -> SyntaxNode {
        SyntaxNode {
            storage: Arc::clone(&self.0),
            at: 0,
        }
    }
}
