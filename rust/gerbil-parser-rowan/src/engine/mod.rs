//! Generated-table execution owners for the Rowan runtime.

mod event_tree;
mod lexer;
mod model;
mod parser;
mod rowan_tree;
mod validation;

pub use event_tree::build_rowan_events;
pub use model::{
    ActionEntry, Diagnostic, GerbilLanguage, GotoEntry, KindCategory, KindSpec, LanguageSpec,
    LexicalExpr, LexicalRule, Operand, OperandAction, Parse, ParseError, ParseReceipt,
    ParserAction, Production, Reduction, ScannedToken, SelectiveGlrReceipt, Symbol, SyntaxKind,
    SyntaxNode, SyntaxToken, Terminal, TerminalSpec, TreeEvent,
};
pub use parser::{parse, parse_scanned};

#[cfg(test)]
#[path = "../../tests/unit/lexical.rs"]
mod lexical_tests;

#[cfg(test)]
#[path = "../../tests/unit/selective_glr.rs"]
mod selective_glr_tests;

#[cfg(test)]
#[path = "../../tests/unit/event_tree.rs"]
mod event_tree_tests;
