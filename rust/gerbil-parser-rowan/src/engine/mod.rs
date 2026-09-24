//! Generated-table execution owners for the Rowan runtime.

mod event_tree;
mod generated_events;
mod graph_projection;
mod lexer;
mod model;
mod parser;
mod rowan_tree;
mod structural_key_line;
mod structural_lines;
mod structural_list;
mod structural_table;
mod validation;

pub use event_tree::build_rowan_events;
pub use generated_events::parse_generated_events;
pub use graph_projection::{
    GraphFieldMode, GraphFieldRule, GraphFieldValue, GraphNodeRule, GraphProjectionSpec,
    GraphRecord, project_syntax_graph,
};
pub use model::{
    ActionEntry, BlockContents, BlockHeaderRule, BlockLineRule, Diagnostic, GerbilLanguage,
    GotoEntry, HeadingFieldsRule, HeadingLineRule, InlineLinkRule, KeyLineContext, KeyLineMode,
    KeyLineRule, KeyValueLineRule, KindCategory, KindSpec, LanguageSpec, LexicalExpr, LexicalRule,
    LineStructureSpec, ListLineRule, Operand, OperandAction, Parse, ParseError, ParseReceipt,
    ParserAction, Production, Reduction, ScannedToken, SelectiveGlrReceipt, Symbol, SyntaxKind,
    SyntaxNode, SyntaxToken, TableLineRule, Terminal, TerminalSpec, TreeEvent, UnclosedBlockPolicy,
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

#[cfg(test)]
#[path = "../../tests/unit/pure_function_aot.rs"]
mod pure_function_aot_tests;
