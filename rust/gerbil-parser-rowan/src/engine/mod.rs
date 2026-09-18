//! Generated-table execution owners for the Rowan runtime.

mod lexer;
mod model;
mod parser;
mod rowan_tree;
mod validation;

pub use model::{
    ActionEntry, Diagnostic, GerbilLanguage, GotoEntry, KindCategory, KindSpec, LanguageSpec,
    LexicalExpr, LexicalRule, Operand, OperandAction, Parse, ParseError, ParseReceipt,
    ParserAction, Production, Reduction, SelectiveGlrReceipt, Symbol, SyntaxKind, SyntaxNode,
    SyntaxToken, Terminal, TerminalSpec,
};
pub use parser::parse;

#[cfg(test)]
#[path = "../../tests/unit/lexical.rs"]
mod lexical_tests;

#[cfg(test)]
#[path = "../../tests/unit/selective_glr.rs"]
mod selective_glr_tests;
