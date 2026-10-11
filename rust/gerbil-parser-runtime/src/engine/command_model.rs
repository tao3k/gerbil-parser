//! Closed command product types and the engine token/word host boundary.
use super::{Diagnostic, ProjectedValue, TextProfile};

/// Closed token-selection algebra; literals are source text, never procedures.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CommandTrigger {
    None,
    Manual(Option<&'static Self>),
    Token {
        kind: &'static str,
        literals: &'static [&'static str],
    },
    Role(&'static str),
    Or(&'static [Self]),
    Lookahead(&'static [Self]),
    Adjacent(&'static [Self]),
}
/// One ordered branch of a command choice.
#[derive(Clone, Copy, Debug)]
pub struct CommandChoice {
    pub trigger: CommandTrigger,
    pub program: &'static [CommandInstruction],
}
/// Closed command construction and control-flow operations.
#[derive(Clone, Copy, Debug)]
pub enum CommandInstruction {
    As(&'static str),
    Raw(&'static str),
    Word(&'static str),
    Command(&'static str),
    Take {
        field: &'static str,
        trigger: CommandTrigger,
    },
    List {
        field: &'static str,
        until: CommandTrigger,
        allow_empty: bool,
    },
    Call {
        field: &'static str,
        form: &'static str,
    },
    Node {
        field: &'static str,
        kind: &'static str,
        program: &'static [Self],
    },
    Optional {
        trigger: CommandTrigger,
        program: &'static [Self],
    },
    Many {
        trigger: CommandTrigger,
        program: &'static [Self],
    },
    Until {
        trigger: CommandTrigger,
        program: &'static [Self],
    },
    Branch {
        trigger: CommandTrigger,
        yes: &'static [Self],
        no: &'static [Self],
    },
    Choose(&'static [CommandChoice]),
    Balance {
        open: CommandTrigger,
        close: CommandTrigger,
        depth: usize,
        open_field: &'static str,
        close_field: &'static str,
        program: &'static [Self],
    },
}
/// A named declared form; selector precedence is resolved by the driver.
#[derive(Clone, Copy, Debug)]
pub struct CommandForm {
    pub id: &'static str,
    pub trigger: CommandTrigger,
    pub priority: u16,
    pub kind: &'static str,
    pub program: &'static [CommandInstruction],
}
/// A token class within one declared command role.
#[derive(Clone, Copy, Debug)]
pub struct CommandTokenClass {
    pub kind: &'static str,
    pub literals: &'static [&'static str],
}
/// Engine role membership lowered from a language declaration.
#[derive(Clone, Copy, Debug)]
pub struct CommandRole {
    pub name: &'static str,
    pub classes: &'static [CommandTokenClass],
}
/// Immutable node roles and forms lowered from the Scheme `CommandProfile`.
#[derive(Clone, Copy, Debug)]
pub struct CommandProgramSpec {
    pub assignment_tail: &'static str,
    pub pipeline_head: &'static str,
    pub forms: &'static [CommandForm],
    pub nodes: &'static [(&'static str, &'static str)],
    pub roles: &'static [CommandRole],
    pub descriptor: &'static TextProfile,
}
/// Engine-owned token/Word boundary. Source uses continuation requests instead.
pub(crate) trait CommandHost<'source> {
    fn source(&self) -> &'source str;
    fn start(&mut self) -> Result<usize, Diagnostic>;
    fn end(&self) -> usize;
    fn position(&self) -> usize;
    fn available(&mut self) -> Result<bool, Diagnostic>;
    fn matches(&mut self, trigger: CommandTrigger) -> Result<bool, Diagnostic>;
    fn raw(&mut self) -> Result<ProjectedValue<'source>, Diagnostic>;
    fn word(&mut self) -> Result<ProjectedValue<'source>, Diagnostic>;
}
