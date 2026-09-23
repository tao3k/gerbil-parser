//! Generated-table execution owners for the Rowan runtime.

mod event_tree;
mod lexer;
mod model;
mod parser;
mod rowan_tree;
mod structural_lines;
mod validation;

pub use event_tree::build_rowan_events;
pub use model::{
    ActionEntry, BlockHeaderRule, BlockLineRule, Diagnostic, GerbilLanguage, GotoEntry,
    HeadingLineRule, KeyValueLineRule, KindCategory, KindSpec, LanguageSpec, LexicalExpr,
    LexicalRule, LineStructureSpec, Operand, OperandAction, Parse, ParseError, ParseReceipt,
    ParserAction, Production, Reduction, ScannedToken, SelectiveGlrReceipt, Symbol, SyntaxKind,
    SyntaxNode, SyntaxToken, Terminal, TerminalSpec, TreeEvent, UnclosedBlockPolicy,
};
pub use parser::{parse, parse_scanned};
pub use structural_lines::parse_structural_lines;

#[cfg(test)]
#[path = "../../tests/unit/lexical.rs"]
mod lexical_tests;

#[cfg(test)]
#[path = "../../tests/unit/selective_glr.rs"]
mod selective_glr_tests;

#[cfg(test)]
#[path = "../../tests/unit/event_tree.rs"]
mod event_tree_tests;

#[cfg(test)]
#[path = "../../tests/unit/structural_lines.rs"]
mod structural_lines_tests;
