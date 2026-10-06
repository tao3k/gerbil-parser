//! Engine-owned Source cursor for closed command programs and Word recognition.
use super::command_program::{
    CommandHost, CommandInstruction, CommandTrigger, PreparedCommandProgram,
};
use super::{
    Diagnostic, PreparedPartProfile, ProjectedNode, ProjectedValue, ResultChildCapture,
    ScannedToken, TreeEvent,
};
use std::collections::{HashMap, VecDeque};

/// Recognized Source and FIFO deferred-body links. Fields retain declaration order.
pub struct CommandParse<'s> {
    pub root: ProjectedNode<'s>,
    pub here_documents: Vec<(usize, usize)>,
    trivia: Vec<(u16, std::ops::Range<usize>)>,
}
fn error(at: usize, message: &str) -> Diagnostic {
    Diagnostic {
        reason_kind: "invalid-command-source",
        byte_offset: at,
        message: message.into(),
    }
}
impl CommandParse<'_> {
    /// Publish the complete token tape, inserting scanner trivia at node boundaries.
    /// # Errors
    /// Rejects overlapping captures or trivia outside the root.
    pub fn events(&self) -> Result<Vec<TreeEvent>, Diagnostic> {
        enum Walk<'a> {
            Node(&'a ProjectedNode<'a>),
            Value(&'a ProjectedValue<'a>),
            Finish(usize),
        }
        let mut tasks = vec![Walk::Node(&self.root)];
        let mut events = Vec::new();
        let mut trivia = self.trivia.iter().peekable();
        let mut offset = 0;
        while let Some(task) = tasks.pop() {
            let boundary = match task {
                Walk::Node(n) | Walk::Value(ProjectedValue::Node(n)) => n.span().start,
                Walk::Value(ProjectedValue::Token { span, .. }) => span.start,
                Walk::Finish(end) => end,
            };
            while let Some((kind, span)) = trivia.peek() {
                if span.end > boundary {
                    break;
                }
                if span.start != offset {
                    return Err(error(offset, "source trivia does not cover gap"));
                }
                events.push(TreeEvent::Token {
                    kind: *kind,
                    start: span.start,
                    end: span.end,
                });
                offset = span.end;
                trivia.next();
            }
            match task {
                Walk::Node(node) => {
                    if node.span().start != offset {
                        return Err(error(offset, "command node is outside source order"));
                    }
                    events.push(TreeEvent::StartNode(node.kind()));
                    tasks.push(Walk::Finish(node.span().end));
                    tasks.extend(node.children().iter().rev().map(|c| Walk::Value(c.value())));
                }
                Walk::Value(ProjectedValue::Node(node)) => tasks.push(Walk::Node(node)),
                Walk::Value(ProjectedValue::Token { kind, span }) => {
                    if span.start != offset {
                        return Err(error(offset, "command token is outside source order"));
                    }
                    events.push(TreeEvent::Token {
                        kind: *kind,
                        start: span.start,
                        end: span.end,
                    });
                    offset = span.end;
                }
                Walk::Finish(end) => {
                    if end != offset {
                        return Err(error(offset, "unclaimed significant source range"));
                    }
                    events.push(TreeEvent::FinishNode);
                }
            }
        }
        if trivia.next().is_some() {
            return Err(error(offset, "trivia remains outside source"));
        }
        Ok(events)
    }
}
struct Cursor<'p, 's> {
    plan: &'p PreparedCommandProgram,
    source: &'s str,
    tokens: &'p [ScannedToken],
    parts: &'p PreparedPartProfile,
    kinds: &'p HashMap<&'static str, u16>,

    at: usize,
    end: usize,
    depth: usize,
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
            depth: 0,
            markers: VecDeque::new(),
            links: Vec::new(),
        };
        let children = cursor.children(CommandTrigger::None)?;
        if !cursor.markers.is_empty() {
            return Err(error(cursor.end, "missing deferred body"));
        }
        let root = cursor.build("BashFile", 0, source.len(), children)?;
        let trivia = tokens
            .iter()
            .filter(|t| cursor.role("trivia", t))
            .map(|t| (cursor.kinds[t.terminal], t.start..t.end))
            .collect();
        Ok(CommandParse {
            root,
            here_documents: cursor.links,
            trivia,
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
    fn simple(&mut self) -> Result<ProjectedNode<'s>, Diagnostic> {
        let first = self
            .peek()
            .ok_or_else(|| error(self.end, "expected command"))?;
        if self.role("reserved", &first) {
            return Err(error(first.start, "unexpected reserved token"));
        }
        let mut named = false;
        let mut children = Vec::new();
        while let Some(token) = self.peek() {
            if self.descriptor_before() {
                let d = self.token()?;
                children.push(node("redirect", self.redirection(Some(d))?));
            } else if self.role("redirect", &token) {
                children.push(node("redirect", self.redirection(None)?));
            } else if token.terminal == "word" {
                if !named
                    && let Some(assignment) =
                        self.parts.assignment(self.source, token.start..token.end)?
                {
                    self.token()?;
                    let form = &self.plan.spec.forms[self.plan.forms["array-tail"]];
                    let assignment = if self
                        .tokens
                        .get(self.at)
                        .is_some_and(|n| n.start == token.end)
                        && self.test(form.trigger, self.at)
                    {
                        let plan = self.plan;
                        plan.execute_initial(
                            form,
                            self,
                            assignment.span().start,
                            vec![node("assignment", assignment)],
                        )?
                    } else {
                        assignment
                    };
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
            return Err(error(first.start, "expected command or redirection"));
        }
        self.build("SimpleCommand", first.start, self.end, children)
    }
    fn compound(&mut self) -> Result<ProjectedNode<'s>, Diagnostic> {
        self.peek();
        let Some(id) = self.selected() else {
            return self.simple();
        };
        let plan = self.plan;
        let command = plan.execute(id, self)?;
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
        if children.len() == 1 {
            let ProjectedValue::Node(command) = children.remove(0).value else {
                unreachable!()
            };
            Ok(command)
        } else {
            self.build("RedirectedCommand", start, self.end, children)
        }
    }
    fn pipeline(&mut self) -> Result<ProjectedNode<'s>, Diagnostic> {
        let start = self.start()?;
        let form = &self.plan.spec.forms[self.plan.forms["pipeline-prefix"]];
        let mut children = Vec::new();
        if form.program.iter().any(|i| match i {
            CommandInstruction::Optional { trigger, .. }
            | CommandInstruction::Many { trigger, .. } => self.test(*trigger, self.at),
            _ => false,
        }) {
            let plan = self.plan;
            children = plan
                .execute_initial(form, self, start, Vec::new())?
                .into_captures();
        }
        let first = self.compound()?;
        children.push(node("command", first));
        while self.peek().is_some_and(|t| self.role("pipeline", &t)) {
            let value = self.raw()?;
            children.push(child("operator", value));
            children.push(node("command", self.compound()?));
        }
        if children.len() == 1 {
            let ProjectedValue::Node(first) = children.remove(0).value else {
                unreachable!()
            };
            Ok(first)
        } else {
            self.build("Pipeline", start, self.end, children)
        }
    }
    fn and_or(&mut self) -> Result<ProjectedNode<'s>, Diagnostic> {
        let first = self.pipeline()?;
        let start = first.span().start;
        let mut children = vec![node("command", first)];
        while self.peek().is_some_and(|t| self.role("and-or", &t)) {
            children.push(child("operator", self.raw()?));
            children.push(node("command", self.pipeline()?));
        }
        if children.len() == 1 {
            let ProjectedValue::Node(first) = children.remove(0).value else {
                unreachable!()
            };
            Ok(first)
        } else {
            self.build("AndOrList", start, self.end, children)
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
    fn children(
        &mut self,
        until: CommandTrigger,
    ) -> Result<Vec<ResultChildCapture<'s>>, Diagnostic> {
        let mut children = Vec::new();
        while let Some(token) = self.peek() {
            if self.test(until, self.at) {
                break;
            }
            let before = self.at;
            if self.role("separator", &token) {
                if token.terminal == "operator"
                    && !children
                        .iter()
                        .any(|c: &ResultChildCapture<'s>| c.field == "command")
                {
                    return Err(error(token.start, "separator has no command"));
                }
                children.push(child("separator", self.raw()?));
                if token.terminal == "newline" {
                    children.extend(self.here()?);
                }
            } else {
                children.push(node("command", self.and_or()?));
            }
            if self.at <= before {
                return Err(error(token.start, "Source did not advance"));
            }
        }
        Ok(children)
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
    fn command(&mut self) -> Result<ProjectedValue<'s>, Diagnostic> {
        if self.depth >= 256 {
            return Err(error(self.end, "Source nesting exceeds engine limit"));
        }
        self.depth += 1;
        let value = self.compound();
        self.depth -= 1;
        value.map(ProjectedValue::Node)
    }
    fn list(
        &mut self,
        until: CommandTrigger,
        allow_empty: bool,
    ) -> Result<ProjectedValue<'s>, Diagnostic> {
        if self.depth >= 256 {
            return Err(error(self.end, "Source nesting exceeds engine limit"));
        }
        let start = self.end;
        self.depth += 1;
        let children = self.children(until);
        self.depth -= 1;
        let children = children?;
        if !allow_empty && !children.iter().any(|c| c.field == "command") {
            return Err(error(start, "empty compound list"));
        }
        self.build("CommandList", start, self.end, children)
            .map(ProjectedValue::Node)
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
