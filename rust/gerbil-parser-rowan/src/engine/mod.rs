//! Generated-table execution owners for the Rowan runtime.

mod contextual_parser;
mod contextual_plan;
pub(crate) mod event_tree;
mod generated_events;
mod graph_index;
mod graph_projection;
mod lexer;
mod model;
mod module_source;
mod parser;
mod rowan_tree;
mod structural_block_opening;
mod structural_key_line;
mod structural_lines;
mod structural_list;
mod structural_table;
mod unicode_alphabetic;
mod unicode_numeric;
mod unicode_whitespace;
mod validation;

pub use contextual_parser::{ContextualParserSpec, parse_contextual};
pub use event_tree::{build_rowan_events, build_rowan_events_catalog};
pub use generated_events::parse_generated_events;
pub use graph_index::{GraphIndex, GraphIndexError, GraphRelation};
pub use graph_projection::{
    GraphFieldMode, GraphFieldRule, GraphFieldValue, GraphNodeRule, GraphProjectionSpec,
    GraphRecord, project_syntax_graph,
};
pub use lexer::PreparedTextProfile;
pub use model::{
    ActionEntry, BlockContents, BlockHeaderRule, BlockLineRule, BlockOpeningMode, Diagnostic,
    EventCatalog, GerbilLanguage, GotoEntry, HeadingFieldsRule, HeadingLineRule, InlineLinkRule,
    KeyLineContext, KeyLineMode, KeyLineRule, KeyValueLineRule, KindCategory, KindSpec,
    LanguageSpec, LexicalExpr, LexicalRule, LineStructureSpec, ListLineRule, ModuleTextProfile,
    Operand, OperandAction, Parse, ParseError, ParseReceipt, ParserAction, Production, Reduction,
    ScannedToken, SelectiveGlrReceipt, Symbol, SyntaxKind, SyntaxNode, SyntaxToken, TableLineRule,
    Terminal, TerminalSpec, TextClass, TextProfile, TreeEvent, UnclosedBlockPolicy,
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

#[cfg(test)]
#[path = "../../tests/unit/event_strategy_aot.rs"]
mod event_strategy_aot_tests;

#[cfg(test)]
#[path = "../../tests/unit/event_fold_aot.rs"]
mod event_fold_aot_tests;

#[cfg(test)]
#[path = "../../tests/unit/graph_index.rs"]
mod graph_index_tests;

#[cfg(test)]
#[path = "../../tests/unit/header_delimiters.rs"]
mod header_delimiter_tests;

#[cfg(test)]
#[path = "../../tests/unit/binding_names.rs"]
mod binding_names_tests;
