//! Generated-table execution owners for the parser runtime.

mod command_model;
mod command_program;
mod command_source;
pub use command_source::{CommandParse, CommandSourceSpec, PreparedCommandSource};
mod contextual_parser;
pub use command_program::{
    CommandChoice, CommandForm, CommandInstruction, CommandProgramSpec, CommandRole,
    CommandTokenClass, CommandTrigger, PreparedCommandProgram,
};
mod contextual_plan;
pub(crate) mod event_tree;
mod generated_events;
mod graph_index;
mod graph_projection;
mod lexer;
mod model;
mod module_source;
mod parser;
mod part_profile;
mod word_parts;
pub use part_profile::{
    BindingSpec, PartGuard, PartOpcode, PartProfileSpec, PartRule, PreparedPartProfile,
    SubscriptSpec,
};
mod recognition_index;
mod result_projection;
mod structural_block_opening;
mod structural_key_line;
mod structural_lines;
mod structural_list;
mod structural_table;
mod unicode_alphabetic;
mod unicode_numeric;
mod unicode_whitespace;
pub(crate) use unicode_whitespace::is_scheme_whitespace;
mod validation;

pub use contextual_parser::{ContextualParserSpec, parse_contextual};
pub use event_tree::{build_syntax_events, build_syntax_events_catalog};
pub use generated_events::parse_generated_events;
pub use graph_index::{GraphIndex, GraphIndexError, GraphRelation};
pub use graph_projection::{
    GraphFieldMode, GraphFieldRule, GraphFieldValue, GraphNodeRule, GraphProjectionSpec,
    GraphRecord, project_syntax_graph,
};
pub use lexer::PreparedTextProfile;
pub use model::{
    ActionEntry, BlockContents, BlockHeaderRule, BlockLineRule, BlockOpeningMode, Diagnostic,
    EventCatalog, GotoEntry, HeadingFieldsRule, HeadingLineRule, InlineLinkRule, KeyLineContext,
    KeyLineMode, KeyLineRule, KeyValueLineRule, KindCategory, KindSpec, LanguageSpec, LexicalExpr,
    LexicalRule, LineStructureSpec, ListLineRule, ModuleTextProfile, Operand, OperandAction, Parse,
    ParseError, ParseReceipt, ParserAction, Production, Reduction, ScannedToken,
    SelectiveGlrReceipt, Symbol, SyntaxKind, SyntaxNode, SyntaxToken, TableLineRule, Terminal,
    TerminalSpec, TextClass, TextProfile, TreeEvent, UnclosedBlockPolicy,
};
pub use parser::{parse, parse_scanned};
pub use result_projection::{
    CaptureKind, CaptureSpec, PreparedResultNode, PreparedResultProfile, PreparedResultProjection,
    ProjectedChild, ProjectedNode, ProjectedValue, ProjectionInstruction, ProjectionOpcode,
    ResultCapture, ResultChildCapture, ResultNodeSpec, ResultProfileSpec, ResultProjection,
};
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

#[cfg(test)]
#[path = "../../tests/unit/result_projection.rs"]
mod result_projection_tests;

#[cfg(test)]
#[path = "../../tests/unit/word_parts.rs"]
mod word_parts_tests;

#[cfg(test)]
#[path = "../../tests/unit/ordered_results.rs"]
mod ordered_results_tests;

#[cfg(test)]
#[path = "../../tests/unit/command_source.rs"]
mod command_source_tests;
