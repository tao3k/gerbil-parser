//! Public generated-table schema and internal parse values.

use rowan::GreenNode;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum KindCategory {
    Node,
    Token,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct KindSpec {
    pub name: &'static str,
    pub category: KindCategory,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct TerminalSpec {
    pub name: &'static str,
    pub syntax_kind: u16,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LexicalExpr {
    Whitespace,
    HorizontalWhitespace,
    Newline,
    DecimalDigits,
    Number,
    NumberLiteral {
        prefixes: &'static [&'static str],
        separator: &'static str,
        suffixes: &'static [&'static str],
        leading_period: bool,
        trailing_period: bool,
    },
    Identifier,
    QuotedString(&'static [&'static str]),
    Heredoc,
    LineComment(&'static [&'static str]),
    BlockComment {
        opening: &'static str,
        closing: &'static str,
    },
    NestedBlockComment {
        opening: &'static str,
        closing: &'static str,
    },
    Choice(&'static [LexicalExpr]),
    Literals(&'static [&'static str]),
    Fallback,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct LexicalRule {
    pub terminal: &'static str,
    pub expression: LexicalExpr,
    pub precedence: i32,
    pub extra: bool,
}

/// One token emitted by a downstream AOT-generated scanner.
///
/// Ranges are UTF-8 byte offsets into the exact source passed to
/// [`crate::parse_scanned`]. The runtime validates complete, ordered coverage.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ScannedToken {
    pub terminal: &'static str,
    pub start: usize,
    pub end: usize,
}

/// Structural decisions emitted by a downstream Scheme-AOT parser.
///
/// The generated language specification owns kind identities. Tokens must
/// cover the source exactly once, in order; nodes may nest to arbitrary depth.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TreeEvent {
    StartNode(u16),
    Token { kind: u16, start: usize, end: usize },
    FinishNode,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Terminal {
    Eof,
    Literal(&'static str),
    Token(&'static str),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ParserAction {
    Shift(u32),
    Reduce(u32),
    Accept,
    RejectNonAssoc,
    Fork(&'static [ParserAction]),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ActionEntry {
    pub terminal: Terminal,
    pub action: ParserAction,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct GotoEntry {
    pub nonterminal: &'static str,
    pub state: u32,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Symbol {
    Terminal(Terminal),
    Nonterminal(&'static str),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum OperandAction {
    Field(&'static str),
    Alias(u16),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Operand {
    pub symbol: Symbol,
    pub actions: &'static [OperandAction],
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Reduction {
    Pass,
    Concat,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Production {
    pub lhs: &'static str,
    pub rhs: &'static [Operand],
    pub reduction: Reduction,
    pub dynamic_precedence: i32,
}

#[derive(Clone, Copy, Debug)]
pub struct LanguageSpec {
    pub language: &'static str,
    pub version: &'static str,
    pub contract: &'static str,
    pub grammar_digest: &'static str,
    pub case_insensitive: bool,
    pub root_kind: u16,
    pub kinds: &'static [KindSpec],
    pub terminals: &'static [TerminalSpec],
    pub lexical_rules: &'static [LexicalRule],
    pub actions: &'static [&'static [ActionEntry]],
    pub gotos: &'static [&'static [GotoEntry]],
    pub productions: &'static [Production],
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct SyntaxKind(pub u16);

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum GerbilLanguage {}

impl rowan::Language for GerbilLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        SyntaxKind(raw.0)
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.0)
    }
}

pub type SyntaxNode = rowan::SyntaxNode<GerbilLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<GerbilLanguage>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParseReceipt {
    pub language: &'static str,
    pub version: &'static str,
    pub contract: &'static str,
    pub grammar_digest: &'static str,
    /// Digest of the downstream Scheme scanner declaration when one is used.
    pub scanner_digest: Option<&'static str>,
    pub source_digest: String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SelectiveGlrReceipt {
    pub branch_budget: usize,
    pub branches_explored: usize,
    pub speculative_branches_explored: usize,
    pub max_speculative_depth: usize,
    pub merged_branches: usize,
    pub successful_completions: usize,
    pub distinct_completions: usize,
    pub winner_reason: &'static str,
    pub dynamic_score: i32,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Diagnostic {
    pub reason_kind: &'static str,
    pub byte_offset: usize,
    pub message: String,
}

#[derive(Clone, Debug)]
pub struct Parse {
    pub(crate) green: GreenNode,
    pub(crate) kinds: &'static [KindSpec],
    pub(crate) receipt: ParseReceipt,
    pub(crate) selective_glr: SelectiveGlrReceipt,
}

impl Parse {
    #[must_use]
    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green.clone())
    }

    #[must_use]
    pub fn receipt(&self) -> &ParseReceipt {
        &self.receipt
    }

    #[must_use]
    pub fn selective_glr_receipt(&self) -> &SelectiveGlrReceipt {
        &self.selective_glr
    }

    #[must_use]
    pub fn kind_name(&self, kind: SyntaxKind) -> Option<&str> {
        self.kinds.get(usize::from(kind.0)).map(|kind| kind.name)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParseError {
    pub receipt: ParseReceipt,
    pub diagnostic: Box<Diagnostic>,
    pub selective_glr: Option<Box<SelectiveGlrReceipt>>,
}

#[derive(Clone, Debug)]
pub(crate) struct ParserFailure {
    pub(crate) diagnostic: Diagnostic,
    pub(crate) selective_glr: Option<Box<SelectiveGlrReceipt>>,
}

#[derive(Clone, Debug)]
pub(crate) struct Token<'source> {
    pub(crate) terminal: &'static str,
    pub(crate) syntax_kind: u16,
    pub(crate) text: &'source str,
    pub(crate) start: usize,
    pub(crate) end: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Child {
    pub(crate) field: Option<&'static str>,
    pub(crate) value: Value,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum Value {
    Token(usize),
    Node {
        kind: u16,
        start: usize,
        end: usize,
        children: Vec<Child>,
    },
    Fragment(Vec<Child>),
}

impl Value {
    pub(crate) fn start(&self, tokens: &[Token<'_>], fallback: usize) -> usize {
        match self {
            Self::Token(index) => tokens[*index].start,
            Self::Node { start, .. } => *start,
            Self::Fragment(children) => children
                .first()
                .map_or(fallback, |child| child.value.start(tokens, fallback)),
        }
    }

    pub(crate) fn end(&self, tokens: &[Token<'_>], fallback: usize) -> usize {
        match self {
            Self::Token(index) => tokens[*index].end,
            Self::Node { end, .. } => *end,
            Self::Fragment(children) => children
                .last()
                .map_or(fallback, |child| child.value.end(tokens, fallback)),
        }
    }

    pub(crate) fn into_children(self) -> Vec<Child> {
        match self {
            Self::Fragment(children) => children,
            value => vec![Child { field: None, value }],
        }
    }
}
