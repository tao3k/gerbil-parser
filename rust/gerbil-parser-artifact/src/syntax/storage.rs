//! Flat source ranges and navigation indexes; token strings are not duplicated.
use std::sync::Arc;
/// Published syntax kind index.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct SyntaxKind(pub u16);
/// UTF-8 byte offset within an owned syntax source.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct TextSize(pub u32);
impl From<u32> for TextSize {
    fn from(n: u32) -> Self {
        Self(n)
    }
}
impl From<TextSize> for u32 {
    fn from(n: TextSize) -> Self {
        n.0
    }
}
impl From<TextSize> for usize {
    fn from(n: TextSize) -> Self {
        n.0 as Self
    }
}
/// Half-open source range used by indexed AST access and graph projections.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct TextRange {
    start: TextSize,
    end: TextSize,
}
impl TextRange {
    /// Construct an ordered source span.
    #[must_use]
    /// # Panics
    /// Panics when the end precedes the start.
    pub fn new(start: TextSize, end: TextSize) -> Self {
        assert!(start <= end);
        Self { start, end }
    }
    /// Construct an empty span at an offset.
    #[must_use]
    pub fn empty(at: TextSize) -> Self {
        Self::new(at, at)
    }
    /// First byte offset.
    #[must_use]
    pub fn start(self) -> TextSize {
        self.start
    }
    /// Exclusive final byte offset.
    #[must_use]
    pub fn end(self) -> TextSize {
        self.end
    }
}
/// One producer-owned node or token range and its structural links.
#[derive(Clone, Debug)]
pub struct IndexedEntry {
    pub kind: u16,
    pub token: bool,
    pub start: usize,
    pub end: usize,
    pub parent: Option<usize>,
    pub subtree_end: usize,
}
#[derive(Debug)]
pub(super) struct Storage {
    pub source: Arc<str>,
    pub entries: Vec<IndexedEntry>,
}
/// One immutable source allocation with a flat navigation index.
#[derive(Clone, Debug)]
pub struct SyntaxTree(pub(super) Arc<Storage>);
impl SyntaxTree {
    /// Admit the producer's ordered flat index without copying any token text.
    /// # Errors
    /// Rejects malformed ranges, parent references, subtree intervals or coverage.
    pub fn from_index(source: &str, entries: Vec<IndexedEntry>) -> Result<Self, String> {
        if source.len() > u32::MAX as usize
            || entries.is_empty()
            || entries[0].token
            || entries[0].parent.is_some()
            || entries[0].start != 0
            || entries[0].end != source.len()
            || entries[0].subtree_end != entries.len()
        {
            return Err("invalid indexed syntax root".into());
        }
        let mut offset = 0;
        let mut ancestors: Vec<usize> = Vec::new();
        for (i, row) in entries.iter().enumerate() {
            while ancestors
                .last()
                .is_some_and(|&owner| entries[owner].subtree_end == i)
            {
                ancestors.pop();
            }
            if row.parent != ancestors.last().copied() || row.start != offset {
                return Err("invalid indexed syntax preorder".into());
            }
            if !row.token {
                ancestors.push(i);
            }
            if row.start > row.end
                || source.get(row.start..row.end).is_none()
                || row.subtree_end <= i
                || row.subtree_end > entries.len()
            {
                return Err("invalid indexed syntax span".into());
            }
            if let Some(parent) = row.parent {
                let Some(owner) = entries.get(parent) else {
                    return Err("invalid indexed syntax parent".into());
                };
                if parent >= i
                    || owner.token
                    || owner.subtree_end <= i
                    || owner.subtree_end < row.subtree_end
                    || owner.start > row.start
                    || owner.end < row.end
                {
                    return Err("invalid indexed syntax ancestry".into());
                }
            } else if i != 0 {
                return Err("extra indexed syntax root".into());
            }
            if row.subtree_end < entries.len() && entries[row.subtree_end].start != row.end {
                return Err("invalid indexed syntax subtree coverage".into());
            }
            if row.token {
                if row.start != offset || row.end <= row.start || row.subtree_end != i + 1 {
                    return Err("invalid indexed syntax token coverage".into());
                }
                offset = row.end;
            }
        }
        if offset != source.len() {
            return Err("incomplete indexed syntax coverage".into());
        }
        Ok(Self(Arc::new(Storage {
            source: Arc::from(source),
            entries,
        })))
    }
    /// Complete original source length.
    #[must_use]
    // Construction admits only source lengths representable by TextSize.
    #[allow(clippy::cast_possible_truncation)]
    pub fn text_len(&self) -> TextSize {
        TextSize(self.0.source.len() as u32)
    }
}
impl std::fmt::Display for SyntaxTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0.source)
    }
}
