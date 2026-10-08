//! Engine-owned Source cursor for closed command programs and Word recognition.
use super::command_program::{
    CommandExecution, CommandHost, CommandInstruction, CommandProgress, CommandRequest,
    CommandTrigger, PreparedCommandProgram,
};
use super::{
    Diagnostic, PreparedPartProfile, ProjectedNode, ProjectedValue, ResultChildCapture,
    ResultProfileSpec, ScannedToken,
};
use std::collections::{HashMap, VecDeque};

mod publication;
use publication::error;
pub use publication::{CommandEvent, CommandParse};

const MAX_SOURCE_FRAMES: usize = 16_384;

enum SourceTask<'s> {
    Compound,
    CompoundAfter,
    Pipeline,
    PipelinePrefixAfter {
        start: usize,
    },
    Simple {
        start: usize,
        named: bool,
        children: Vec<ResultChildCapture<'s>>,
    },
    SimpleAfter {
        start: usize,
        named: bool,
        children: Vec<ResultChildCapture<'s>>,
    },
    PipelineAfter {
        start: usize,
        children: Vec<ResultChildCapture<'s>>,
    },
    AndOr,
    AndOrAfter {
        start: Option<usize>,
        children: Vec<ResultChildCapture<'s>>,
    },
    List(ListFrame<'s>),
    Vm(CommandExecution<'s>),
    VmAfter {
        execution: CommandExecution<'s>,
        field: &'static str,
    },
}
struct ListFrame<'s> {
    start: usize,
    until: CommandTrigger,
    allow_empty: bool,
    root: bool,
    children: Vec<ResultChildCapture<'s>>,
    before: Option<(usize, usize)>,
}
struct SourceExecution<'s> {
    tasks: Vec<SourceTask<'s>>,
    live_frames: usize,
    returned: Option<ProjectedNode<'s>>,
}
impl<'s> SourceExecution<'s> {
    fn schedule(&mut self, task: SourceTask<'s>, at: usize) -> Result<(), Diagnostic> {
        schedule(&mut self.tasks, &mut self.live_frames, task, at)
    }
}
impl SourceTask<'_> {
    fn live_frames(&self) -> usize {
        1 + match self {
            Self::Vm(execution) | Self::VmAfter { execution, .. } => execution.live_frames(),
            _ => 0,
        }
    }
}
fn schedule<'s>(
    tasks: &mut Vec<SourceTask<'s>>,
    live: &mut usize,
    task: SourceTask<'s>,
    at: usize,
) -> Result<(), Diagnostic> {
    let next = live
        .checked_add(task.live_frames())
        .filter(|n| *n <= MAX_SOURCE_FRAMES)
        .ok_or_else(|| Diagnostic {
            reason_kind: "command-source-resource-limit",
            byte_offset: at,
            message: "Source continuation frame budget exceeded".into(),
        })?;
    *live = next;
    tasks.push(task);
    Ok(())
}
fn take_result<'s>(
    value: &mut Option<ProjectedNode<'s>>,
    at: usize,
) -> Result<ProjectedNode<'s>, Diagnostic> {
    value
        .take()
        .ok_or_else(|| error(at, "missing Source continuation result"))
}
struct Cursor<'p, 's> {
    plan: &'p PreparedCommandProgram,
    source: &'s str,
    tokens: &'p [ScannedToken],
    parts: &'p PreparedPartProfile,
    kinds: &'p HashMap<&'static str, u16>,

    at: usize,
    end: usize,
    markers: VecDeque<(usize, bool)>,
    links: Vec<(usize, usize)>,
}
impl PreparedCommandProgram {
    /// Recognize a full Source from a validated scanner tape using declared roles.
    /// The cursor and command/Word/list bridge are owned entirely by the engine.
    /// # Errors
    /// Rejects invalid token coverage, unexpected selectors and unmatched deferred bodies.
    pub fn parse_scanned<'s>(
        &self,
        source: &'s str,
        tokens: &[ScannedToken],
        parts: &PreparedPartProfile,
    ) -> Result<CommandParse<'s>, Diagnostic> {
        if !std::ptr::eq(self.result_owner, parts.result_owner) {
            return Err(error(0, "foreign Word result catalog"));
        }
        let kinds = &self.tokens;
        let mut end = 0;
        for token in tokens {
            if token.start != end
                || token.end <= end
                || source.get(token.start..token.end).is_none()
                || !kinds.contains_key(token.terminal)
            {
                return Err(error(end, "invalid command scanner tape"));
            }
            end = token.end;
        }
        if end != source.len() {
            return Err(error(end, "incomplete command scanner tape"));
        }
        let mut cursor = Cursor {
            plan: self,
            source,
            tokens,
            parts,
            kinds,
            at: 0,
            end: 0,
            markers: VecDeque::new(),
            links: Vec::new(),
        };
        let root = cursor.run_source()?;
        if !cursor.markers.is_empty() {
            return Err(error(cursor.end, "missing deferred body"));
        }
        let trivia = tokens
            .iter()
            .filter(|t| cursor.role("trivia", t))
            .map(|t| (cursor.kinds[t.terminal], t.start..t.end))
            .collect();
        Ok(CommandParse {
            root,
            here_documents: cursor.links,
            trivia,
            source,
            result_owner: self.result_owner,
        })
    }
}
fn child<'s>(field: &'static str, value: ProjectedValue<'s>) -> ResultChildCapture<'s> {
    ResultChildCapture { field, value }
}
fn node<'s>(field: &'static str, value: ProjectedNode<'s>) -> ResultChildCapture<'s> {
    child(field, ProjectedValue::Node(value))
}
impl<'s> Cursor<'_, 's> {
    fn text(&self, token: &ScannedToken) -> &'s str {
        &self.source[token.start..token.end]
    }
    fn role(&self, role: &str, token: &ScannedToken) -> bool {
        self.plan
            .spec
            .roles
            .iter()
            .find(|r| r.name == role)
            .is_some_and(|r| {
                r.classes.iter().any(|c| {
                    c.kind == token.terminal
                        && (c.literals.is_empty() || c.literals.contains(&self.text(token)))
                })
            })
    }
    fn peek(&mut self) -> Option<ScannedToken> {
        while self
            .tokens
            .get(self.at)
            .is_some_and(|t| self.role("trivia", t))
        {
            self.end = self.tokens[self.at].end;
            self.at += 1;
        }
        self.tokens.get(self.at).copied()
    }
    fn token(&mut self) -> Result<ScannedToken, Diagnostic> {
        let token = self
            .peek()
            .ok_or_else(|| error(self.end, "expected command token"))?;
        self.at += 1;
        self.end = token.end;
        Ok(token)
    }
    fn raw_value(&self, token: ScannedToken) -> ProjectedValue<'s> {
        ProjectedValue::Token {
            kind: self.kinds[token.terminal],
            span: token.start..token.end,
        }
    }
    fn test(&self, trigger: CommandTrigger, at: usize) -> bool {
        match trigger {
            CommandTrigger::None | CommandTrigger::Manual(None) => false,
            CommandTrigger::Manual(Some(t)) => self.test(*t, at),
            CommandTrigger::Token { kind, literals } => self.tokens.get(at).is_some_and(|t| {
                t.terminal == kind && (literals.is_empty() || literals.contains(&self.text(t)))
            }),
            CommandTrigger::Role(role) => self.tokens.get(at).is_some_and(|t| self.role(role, t)),
            CommandTrigger::Or(rows) => rows.iter().any(|t| self.test(*t, at)),
            CommandTrigger::Lookahead(rows) | CommandTrigger::Adjacent(rows) => {
                let adjacent = matches!(trigger, CommandTrigger::Adjacent(_));
                let mut pos = at;
                let mut previous = None;
                for row in rows {
                    if !adjacent {
                        while self.tokens.get(pos).is_some_and(|t| self.role("trivia", t)) {
                            pos += 1;
                        }
                    }
                    let Some(token) = self.tokens.get(pos) else {
                        return false;
                    };
                    if adjacent && previous.is_some_and(|end| end != token.start)
                        || !self.test(*row, pos)
                    {
                        return false;
                    }
                    previous = Some(token.end);
                    pos += 1;
                }
                true
            }
        }
    }
    fn build(
        &self,
        role: &str,
        start: usize,
        end: usize,
        children: Vec<ResultChildCapture<'s>>,
    ) -> Result<ProjectedNode<'s>, Diagnostic> {
        self.plan
            .nodes
            .get(role)
            .ok_or_else(|| error(start, "missing Source node role"))?
            .build(self.source, start..end, children)
    }
    fn selected(&self) -> Option<&'static str> {
        let token = self.tokens.get(self.at)?;
        let mut selected = self
            .plan
            .selectors
            .get(token.terminal)
            .and_then(|rows| rows.get(self.text(token)))
            .map(|&index| &self.plan.spec.forms[index]);
        for &index in &self.plan.fallback {
            let form = &self.plan.spec.forms[index];
            if self.test(form.trigger, self.at)
                && selected.is_none_or(|s| form.priority > s.priority)
            {
                selected = Some(form);
            }
        }
        selected.map(|f| f.id)
    }
    fn descriptor_before(&self) -> bool {
        self.tokens.get(self.at).is_some_and(|t| {
            t.terminal == "word"
                && self
                    .plan
                    .descriptor
                    .match_prefix(self.source, t.start, t.end)
                    == Some(t.end)
                && self
                    .tokens
                    .get(self.at + 1)
                    .is_some_and(|n| self.role("redirect", n) && t.end == n.start)
        })
    }
    fn redirection(
        &mut self,
        descriptor: Option<ScannedToken>,
    ) -> Result<ProjectedNode<'s>, Diagnostic> {
        let operator = self.token()?;
        if !self.role("redirect", &operator) {
            return Err(error(operator.start, "expected redirection"));
        }
        let start = descriptor.map_or(operator.start, |t| t.start);
        let mut children = Vec::new();
        if let Some(d) = descriptor {
            children.push(child("descriptor", self.raw_value(d)));
        }
        let here = self.role("here-redirect", &operator);
        children.push(child("operator", self.raw_value(operator)));
        let target = self
            .peek()
            .ok_or_else(|| error(self.end, "missing redirection target"))?;
        let value = if here {
            if target.terminal != "heredoc-marker" {
                return Err(error(target.start, "expected deferred marker"));
            }
            let (_, quoted) = crate::scanner::shell_delimiter(self.text(&target), target.start)?;
            self.markers.push_back((target.start, quoted));
            self.raw()?
        } else {
            self.word()?
        };
        children.push(child("target", value));
        self.build("Redirection", start, self.end, children)
    }
    // One outer loop drives Source and the existing Command VM. Child results
    // move through a single slot and are published once by their continuation.
    fn run_source(&mut self) -> Result<ProjectedNode<'s>, Diagnostic> {
        let mut execution = SourceExecution {
            tasks: Vec::new(),
            live_frames: 0,
            returned: None,
        };
        execution.schedule(
            SourceTask::List(ListFrame {
                start: 0,
                until: CommandTrigger::None,
                allow_empty: true,
                root: true,
                children: Vec::new(),
                before: None,
            }),
            self.end,
        )?;
        while let Some(task) = execution.tasks.pop() {
            execution.live_frames -= task.live_frames();
            self.step_source(task, &mut execution)?;
        }
        take_result(&mut execution.returned, self.end)
    }
    fn step_source(
        &mut self,
        task: SourceTask<'s>,
        execution: &mut SourceExecution<'s>,
    ) -> Result<(), Diagnostic> {
        match task {
            SourceTask::Compound => self.step_compound(execution)?,
            SourceTask::CompoundAfter => self.finish_compound(execution)?,
            SourceTask::Simple {
                start,
                named,
                children,
            } => self.step_simple(start, named, children, execution)?,
            SourceTask::Pipeline => self.step_pipeline(execution)?,
            SourceTask::PipelineAfter { start, children } => {
                self.step_pipeline_after(start, children, execution)?;
            }
            SourceTask::AndOrAfter { start, children } => {
                self.step_and_or_after(start, children, execution)?;
            }
            SourceTask::List(frame) => self.step_list(frame, execution)?,
            SourceTask::Vm(vm) => self.step_vm(vm, execution)?,
            SourceTask::SimpleAfter {
                start,
                named,
                mut children,
            } => {
                children.push(node(
                    "assignment",
                    take_result(&mut execution.returned, self.end)?,
                ));
                execution.schedule(
                    SourceTask::Simple {
                        start,
                        named,
                        children,
                    },
                    self.end,
                )?;
            }
            SourceTask::PipelinePrefixAfter { start } => {
                let children = take_result(&mut execution.returned, self.end)?.into_captures();
                execution.schedule(SourceTask::PipelineAfter { start, children }, self.end)?;
                execution.schedule(SourceTask::Compound, self.end)?;
            }
            SourceTask::AndOr => {
                execution.schedule(
                    SourceTask::AndOrAfter {
                        start: None,
                        children: Vec::new(),
                    },
                    self.end,
                )?;
                execution.schedule(SourceTask::Pipeline, self.end)?;
            }
            SourceTask::VmAfter {
                execution: mut vm,
                field,
            } => {
                vm.publish(field, take_result(&mut execution.returned, self.end)?)?;
                execution.schedule(SourceTask::Vm(vm), self.end)?;
            }
        }
        Ok(())
    }
    fn step_compound(&mut self, execution: &mut SourceExecution<'s>) -> Result<(), Diagnostic> {
        self.peek();
        if let Some(id) = self.selected() {
            let vm = self.plan.begin(id, self.start()?)?;
            execution.schedule(SourceTask::CompoundAfter, self.end)?;
            execution.schedule(SourceTask::Vm(vm), self.end)?;
        } else {
            let first = self
                .peek()
                .ok_or_else(|| error(self.end, "expected command"))?;
            if self.role("reserved", &first) {
                return Err(error(first.start, "unexpected reserved token"));
            }
            execution.schedule(
                SourceTask::Simple {
                    start: first.start,
                    named: false,
                    children: Vec::new(),
                },
                self.end,
            )?;
        }
        Ok(())
    }
    fn finish_compound(&mut self, execution: &mut SourceExecution<'s>) -> Result<(), Diagnostic> {
        let command = take_result(&mut execution.returned, self.end)?;
        let start = command.span().start;
        let mut children = vec![node("command", command)];
        while let Some(token) = self.peek() {
            if self.descriptor_before() {
                let d = self.token()?;
                children.push(node("redirect", self.redirection(Some(d))?));
            } else if self.role("redirect", &token) {
                children.push(node("redirect", self.redirection(None)?));
            } else {
                break;
            }
        }
        execution.returned = Some(self.finish_chain("RedirectedCommand", start, children)?);
        Ok(())
    }
    fn step_simple(
        &mut self,
        start: usize,
        mut named: bool,
        mut children: Vec<ResultChildCapture<'s>>,
        execution: &mut SourceExecution<'s>,
    ) -> Result<(), Diagnostic> {
        while let Some(token) = self.peek() {
            if self.descriptor_before() {
                let descriptor = self.token()?;
                children.push(node("redirect", self.redirection(Some(descriptor))?));
            } else if self.role("redirect", &token) {
                children.push(node("redirect", self.redirection(None)?));
            } else if token.terminal == "word" {
                if !named
                    && let Some(assignment) =
                        self.parts.assignment(self.source, token.start..token.end)?
                {
                    self.token()?;
                    let form = &self.plan.spec.forms[self.plan.assignment_tail];
                    if self
                        .tokens
                        .get(self.at)
                        .is_some_and(|n| n.start == token.end)
                        && self.test(form.trigger, self.at)
                    {
                        let vm = PreparedCommandProgram::begin_initial(
                            form,
                            assignment.span().start,
                            vec![node("assignment", assignment)],
                        );
                        execution.schedule(
                            SourceTask::SimpleAfter {
                                start,
                                named,
                                children,
                            },
                            self.end,
                        )?;
                        execution.schedule(SourceTask::Vm(vm), self.end)?;
                        return Ok(());
                    }
                    children.push(node("assignment", assignment));
                } else {
                    children.push(child(if named { "argument" } else { "name" }, self.word()?));
                    named = true;
                }
            } else {
                break;
            }
        }
        if children.is_empty() {
            return Err(error(start, "expected command or redirection"));
        }
        execution.returned = Some(self.build("SimpleCommand", start, self.end, children)?);
        Ok(())
    }
    fn step_pipeline(&mut self, execution: &mut SourceExecution<'s>) -> Result<(), Diagnostic> {
        let start = self.start()?;
        let form = &self.plan.spec.forms[self.plan.pipeline_head];
        if form.program.iter().any(|i| match i {
            CommandInstruction::Optional { trigger, .. }
            | CommandInstruction::Many { trigger, .. } => self.test(*trigger, self.at),
            _ => false,
        }) {
            let vm = PreparedCommandProgram::begin_initial(form, start, Vec::new());
            execution.schedule(SourceTask::PipelinePrefixAfter { start }, self.end)?;
            execution.schedule(SourceTask::Vm(vm), self.end)?;
        } else {
            execution.schedule(
                SourceTask::PipelineAfter {
                    start,
                    children: Vec::new(),
                },
                self.end,
            )?;
            execution.schedule(SourceTask::Compound, self.end)?;
        }
        Ok(())
    }
    fn step_pipeline_after(
        &mut self,
        start: usize,
        mut children: Vec<ResultChildCapture<'s>>,
        execution: &mut SourceExecution<'s>,
    ) -> Result<(), Diagnostic> {
        children.push(node(
            "command",
            take_result(&mut execution.returned, self.end)?,
        ));
        if self.peek().is_some_and(|t| self.role("pipeline", &t)) {
            children.push(child("operator", self.raw()?));
            execution.schedule(SourceTask::PipelineAfter { start, children }, self.end)?;
            execution.schedule(SourceTask::Compound, self.end)?;
        } else {
            execution.returned = Some(self.finish_chain("Pipeline", start, children)?);
        }
        Ok(())
    }
    fn step_and_or_after(
        &mut self,
        start: Option<usize>,
        mut children: Vec<ResultChildCapture<'s>>,
        execution: &mut SourceExecution<'s>,
    ) -> Result<(), Diagnostic> {
        let command = take_result(&mut execution.returned, self.end)?;
        let start = start.unwrap_or(command.span().start);
        children.push(node("command", command));
        if self.peek().is_some_and(|t| self.role("and-or", &t)) {
            children.push(child("operator", self.raw()?));
            execution.schedule(
                SourceTask::AndOrAfter {
                    start: Some(start),
                    children,
                },
                self.end,
            )?;
            execution.schedule(SourceTask::Pipeline, self.end)?;
        } else {
            execution.returned = Some(self.finish_chain("AndOrList", start, children)?);
        }
        Ok(())
    }
    fn step_list(
        &mut self,
        frame: ListFrame<'s>,
        execution: &mut SourceExecution<'s>,
    ) -> Result<(), Diagnostic> {
        let ListFrame {
            start,
            until,
            allow_empty,
            root,
            mut children,
            before,
        } = frame;

        if let Some((at, offset)) = before {
            children.push(node(
                "command",
                take_result(&mut execution.returned, self.end)?,
            ));
            if self.at <= at {
                return Err(error(offset, "Source did not advance"));
            }
        }
        while let Some(token) = self.peek() {
            if self.test(until, self.at) {
                break;
            }
            let at = self.at;
            if self.role("separator", &token) {
                if token.terminal == "operator" && !children.iter().any(|c| c.field == "command") {
                    return Err(error(token.start, "separator has no command"));
                }
                children.push(child("separator", self.raw()?));
                if token.terminal == "newline" {
                    children.extend(self.here()?);
                }
                if self.at <= at {
                    return Err(error(token.start, "Source did not advance"));
                }
            } else {
                execution.schedule(
                    SourceTask::List(ListFrame {
                        start,
                        until,
                        allow_empty,
                        root,
                        children,
                        before: Some((at, token.start)),
                    }),
                    self.end,
                )?;
                execution.schedule(SourceTask::AndOr, self.end)?;
                // The list moved to its continuation; no partial result exists.
                execution.returned = None;
                return Ok(());
            }
        }
        if !allow_empty && !children.iter().any(|c| c.field == "command") {
            return Err(error(start, "empty compound list"));
        }
        let end = if root { self.source.len() } else { self.end };
        execution.returned = Some(self.build(
            if root { "BashFile" } else { "CommandList" },
            start,
            end,
            children,
        )?);
        Ok(())
    }
    fn step_vm(
        &mut self,
        mut vm: CommandExecution<'s>,
        execution: &mut SourceExecution<'s>,
    ) -> Result<(), Diagnostic> {
        let plan = self.plan;
        match plan.resume(&mut vm, self, MAX_SOURCE_FRAMES - execution.live_frames - 1)? {
            CommandProgress::Complete(result) => execution.returned = Some(result),
            CommandProgress::Suspend(request) => {
                let (field, next) = match request {
                    CommandRequest::Compound(field) => (field, SourceTask::Compound),
                    CommandRequest::List {
                        field,
                        until,
                        allow_empty,
                    } => (
                        field,
                        SourceTask::List(ListFrame {
                            start: self.end,
                            until,
                            allow_empty,
                            root: false,
                            children: Vec::new(),
                            before: None,
                        }),
                    ),
                };
                execution.schedule(
                    SourceTask::VmAfter {
                        execution: vm,
                        field,
                    },
                    self.end,
                )?;
                execution.schedule(next, self.end)?;
            }
        }
        Ok(())
    }
    fn finish_chain(
        &self,
        kind: &str,
        start: usize,
        mut children: Vec<ResultChildCapture<'s>>,
    ) -> Result<ProjectedNode<'s>, Diagnostic> {
        if children.len() == 1 {
            let ProjectedValue::Node(command) = children.remove(0).value else {
                unreachable!()
            };
            Ok(command)
        } else {
            self.build(kind, start, self.end, children)
        }
    }
    fn here(&mut self) -> Result<Vec<ResultChildCapture<'s>>, Diagnostic> {
        let mut nodes = Vec::new();
        while self
            .peek()
            .is_some_and(|t| matches!(t.terminal, "heredoc-content" | "heredoc-end"))
        {
            let (marker, quoted) = self
                .markers
                .pop_front()
                .ok_or_else(|| error(self.end, "deferred body has no marker"))?;
            let start = self.start()?;
            let mut children = Vec::new();
            loop {
                let token = self.token()?;
                if !matches!(token.terminal, "heredoc-content" | "heredoc-end") {
                    return Err(error(token.start, "unterminated deferred body"));
                }
                let done = token.terminal == "heredoc-end";
                let value = if done || quoted {
                    self.raw_value(token)
                } else {
                    ProjectedValue::Node(
                        self.parts
                            .here_content(self.source, token.start..token.end)?,
                    )
                };
                children.push(child(if done { "delimiter" } else { "content" }, value));
                if done {
                    break;
                }
            }
            self.links.push((marker, start));
            nodes.push(node(
                "here-document",
                self.build("HereDocument", start, self.end, children)?,
            ));
        }
        Ok(nodes)
    }
}
impl<'s> CommandHost<'s> for Cursor<'_, 's> {
    fn source(&self) -> &'s str {
        self.source
    }
    fn start(&mut self) -> Result<usize, Diagnostic> {
        self.peek()
            .map(|t| t.start)
            .ok_or_else(|| error(self.end, "expected command start"))
    }
    fn end(&self) -> usize {
        self.end
    }
    fn position(&self) -> usize {
        self.at
    }
    fn available(&mut self) -> Result<bool, Diagnostic> {
        Ok(self.peek().is_some())
    }
    fn matches(&mut self, trigger: CommandTrigger) -> Result<bool, Diagnostic> {
        self.peek();
        Ok(self.test(trigger, self.at))
    }
    fn raw(&mut self) -> Result<ProjectedValue<'s>, Diagnostic> {
        let token = self.token()?;
        Ok(self.raw_value(token))
    }
    fn word(&mut self) -> Result<ProjectedValue<'s>, Diagnostic> {
        let token = self.token()?;
        if token.terminal != "word" {
            return Err(error(token.start, "expected Word"));
        }
        Ok(ProjectedValue::Node(
            self.parts.word(self.source, token.start..token.end)?,
        ))
    }
}

/// One generated product of the declared scanner, Word, Command and result slots.
#[derive(Clone, Copy, Debug)]
pub struct CommandSourceSpec {
    pub scanner: &'static crate::scanner::ScannerSpec,
    pub parts: &'static super::PartProfileSpec,
    pub regions: &'static crate::scanner::RegionSpec,
    pub results: &'static super::ResultProfileSpec,
    pub commands: &'static super::CommandProgramSpec,
}
/// Reusable admitted Source product. Languages contribute immutable data only.
pub struct PreparedCommandSource {
    spec: &'static CommandSourceSpec,
    commands: PreparedCommandProgram,
    parts: PreparedPartProfile,
}
impl PreparedCommandSource {
    /// Prepare all language slots once and check role terminals against the scanner.
    /// # Errors
    /// Rejects malformed profiles or role terminals absent from the scanner.
    pub fn new(spec: &'static CommandSourceSpec) -> Result<Self, Diagnostic> {
        crate::scanner::ContextualScanner::new(spec.scanner, "")?;
        if !spec.scanner.positions.contains(&"source") {
            return Err(error(0, "scanner lacks Source position"));
        }
        for role in spec.commands.roles {
            if !role
                .classes
                .iter()
                .all(|c| spec.scanner.cells.iter().any(|r| r.terminal == c.kind))
            {
                return Err(error(0, "command role is not declared by scanner"));
            }
        }
        let commands = PreparedCommandProgram::new(spec.commands, spec.results)?;
        if !spec
            .scanner
            .cells
            .iter()
            .all(|cell| commands.tokens.contains_key(cell.terminal))
        {
            return Err(error(0, "scanner terminal is absent from result catalog"));
        }
        let parts = PreparedPartProfile::new(spec.parts, spec.results, spec.regions)?;
        Ok(Self {
            spec,
            commands,
            parts,
        })
    }
    /// Scan and recognize immutable source through the engine-owned cursor.
    /// # Errors
    /// Returns the first scanner or command diagnostic for rejected syntax.
    pub fn parse<'s>(&self, source: &'s str) -> Result<CommandParse<'s>, Diagnostic> {
        let tokens =
            crate::scanner::ContextualScanner::new(self.spec.scanner, source)?.scan("source")?;
        self.commands.parse_scanned(source, &tokens, &self.parts)
    }
}
