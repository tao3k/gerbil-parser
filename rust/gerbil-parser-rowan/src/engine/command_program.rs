//! Closed Scheme command control programs, executed with an explicit frame stack.
use super::{
    Diagnostic, KindCategory, PreparedResultNode, PreparedResultProfile, PreparedTextProfile,
    ProjectedNode, ProjectedValue, ResultChildCapture, ResultProfileSpec,
};
use std::collections::{HashMap, HashSet};

pub(crate) use super::command_model::CommandHost;
pub use super::command_model::{
    CommandChoice, CommandForm, CommandInstruction, CommandProgramSpec, CommandRole,
    CommandTokenClass, CommandTrigger,
};

/// Prepared node constructors and named forms; no per-operation catalog binding.
pub struct PreparedCommandProgram {
    pub(super) spec: &'static CommandProgramSpec,
    pub(super) result_owner: &'static ResultProfileSpec,
    widths: HashMap<&'static str, usize>,
    pub(super) assignment_tail: usize,
    pub(super) pipeline_head: usize,
    pub(super) forms: HashMap<&'static str, usize>,
    pub(super) descriptor: PreparedTextProfile,
    pub(super) tokens: HashMap<&'static str, u16>,
    pub(super) selectors: HashMap<&'static str, HashMap<&'static str, usize>>,
    pub(super) fallback: Vec<usize>,
    pub(super) nodes: HashMap<&'static str, PreparedResultNode>,
}
fn error(at: usize, message: &str) -> Diagnostic {
    Diagnostic {
        reason_kind: "invalid-command-program",
        byte_offset: at,
        message: message.into(),
    }
}
fn trigger_valid(trigger: CommandTrigger, depth: usize) -> bool {
    if depth >= 32 {
        return false;
    }
    match trigger {
        CommandTrigger::None | CommandTrigger::Manual(None) => true,
        CommandTrigger::Manual(Some(t)) => {
            matches!(
                t,
                CommandTrigger::Token {
                    kind: "word" | "operator",
                    ..
                }
            ) && trigger_valid(*t, depth + 1)
        }
        CommandTrigger::Token { kind, literals } => {
            ["word", "operator", "newline"].contains(&kind)
                && literals.iter().all(|s| !s.is_empty())
        }
        CommandTrigger::Role(role) => [
            "trivia",
            "separator",
            "redirect",
            "here-redirect",
            "strip-redirect",
            "reserved",
            "conditional-operator",
            "pipeline",
            "and-or",
            "case-end",
        ]
        .contains(&role),
        CommandTrigger::Or(rows) => {
            !rows.is_empty() && rows.iter().all(|t| trigger_valid(*t, depth + 1))
        }
        CommandTrigger::Lookahead(rows) | CommandTrigger::Adjacent(rows) => {
            !rows.is_empty()
                && rows.len() <= 32
                && rows.iter().all(|t| {
                    matches!(
                        t,
                        CommandTrigger::Token {
                            kind: "word" | "operator",
                            ..
                        }
                    ) && trigger_valid(*t, depth + 1)
                })
        }
    }
}
struct Frame<'s> {
    kind: &'static str,
    start: usize,
    field: Option<&'static str>,
    children: Vec<ResultChildCapture<'s>>,
}
enum Task {
    Program(&'static [CommandInstruction], usize),
    Finish,
    Repeat {
        trigger: CommandTrigger,
        program: &'static [CommandInstruction],
        until: bool,
        before: Option<usize>,
    },
    Balance {
        open: CommandTrigger,
        close: CommandTrigger,
        depth: usize,
        open_field: &'static str,
        close_field: &'static str,
        program: &'static [CommandInstruction],
        before: Option<usize>,
    },
}
impl PreparedCommandProgram {
    /// Admit all forms, field uses, consuming loops and manual-call graph once.
    /// # Errors
    /// Rejects foreign fields, malformed selectors, nonprogressing bodies and call cycles.
    pub fn new(
        spec: &'static CommandProgramSpec,
        results: &'static ResultProfileSpec,
    ) -> Result<Self, Diagnostic> {
        if spec.forms.is_empty() || spec.nodes.is_empty() {
            return Err(error(0, "empty command specification"));
        }
        let results_plan = PreparedResultProfile::new(results)?;
        let mut nodes = HashMap::new();
        for &(role, kind) in spec.nodes {
            if role.is_empty() || nodes.insert(role, results_plan.bind_node(kind)?).is_some() {
                return Err(error(0, "duplicate command node role"));
            }
        }
        let mut forms = HashMap::new();
        for (index, form) in spec.forms.iter().enumerate() {
            if form.id.is_empty()
                || form.priority > 1000
                || !trigger_valid(form.trigger, 0)
                || !nodes.contains_key(form.kind)
                || forms.insert(form.id, index).is_some()
            {
                return Err(error(0, "invalid command form"));
            }
        }
        let mut tokens = HashMap::new();
        for (index, kind) in results
            .kinds
            .iter()
            .enumerate()
            .filter(|(_, k)| k.category == KindCategory::Token)
        {
            tokens.insert(
                kind.name,
                u16::try_from(index).map_err(|_| error(0, "token kind overflow"))?,
            );
        }
        admit_roles(spec, &tokens)?;
        admit_shapes(spec, results)?;
        let descriptor = PreparedTextProfile::new(spec.descriptor)
            .ok_or_else(|| error(0, "invalid descriptor text profile"))?;
        let (selectors, fallback) = admit_selectors(spec)?;
        let assignment_tail = *forms
            .get(spec.assignment_tail)
            .ok_or_else(|| error(0, "missing assignment entrypoint"))?;
        let pipeline_head = *forms
            .get(spec.pipeline_head)
            .ok_or_else(|| error(0, "missing pipeline entrypoint"))?;
        let mut prepared = Self {
            spec,
            result_owner: results,
            widths: HashMap::new(),
            forms,
            assignment_tail,
            pipeline_head,
            descriptor,
            tokens,
            selectors,
            fallback,
            nodes,
        };
        let mut done = HashSet::new();
        let mut order = Vec::new();
        for form in spec.forms {
            prepared.visit(form.id, &mut done, &mut order)?;
        }
        for id in order {
            let form = &spec.forms[prepared.forms[id]];
            let width = prepared.validate(form.program, form.kind, results, 0)?;
            prepared.widths.insert(id, width);
            if !matches!(form.trigger, CommandTrigger::Manual(_)) && width == 0 {
                return Err(error(0, "command form does not consume input"));
            }
        }
        prepared.admit_entries(results)?;
        Ok(prepared)
    }
    fn fields(
        &self,
        role: &str,
        results: &ResultProfileSpec,
    ) -> Result<&'static [&'static str], Diagnostic> {
        let target = self
            .spec
            .nodes
            .iter()
            .find(|(r, _)| *r == role)
            .ok_or_else(|| error(0, "unknown command node role"))?
            .1;
        results
            .nodes
            .iter()
            .find(|n| results.kinds[usize::from(n.kind)].name == target)
            .map(|n| n.fields)
            .ok_or_else(|| error(0, "missing command fields"))
    }
    fn validate(
        &self,
        program: &[CommandInstruction],
        kind: &str,
        results: &ResultProfileSpec,
        depth: usize,
    ) -> Result<usize, Diagnostic> {
        if depth >= 128 {
            return Err(error(0, "invalid command program depth"));
        }
        let fields = self.fields(kind, results)?;
        let field = |name| {
            if fields.contains(&name) {
                Ok(())
            } else {
                Err(error(0, "foreign command field"))
            }
        };
        let trigger = |t| {
            if trigger_valid(t, 0) {
                Ok(())
            } else {
                Err(error(0, "invalid command trigger"))
            }
        };
        let body = |p, k| self.validate(p, k, results, depth + 1);
        let mut width = 0usize;
        for instruction in program {
            let consumed = match *instruction {
                CommandInstruction::As(k) => {
                    if !fields
                        .iter()
                        .all(|f| self.fields(k, results).is_ok_and(|v| v.contains(f)))
                    {
                        return Err(error(0, "projection branch loses fields"));
                    }
                    0
                }
                leaf @ (CommandInstruction::Raw(_)
                | CommandInstruction::Word(_)
                | CommandInstruction::Command(_)
                | CommandInstruction::Take { .. }
                | CommandInstruction::List { .. }
                | CommandInstruction::Call { .. }) => self.validate_leaf(leaf, &field, &trigger)?,
                CommandInstruction::Node {
                    field: f,
                    kind: k,
                    program: p,
                } => {
                    field(f)?;
                    body(p, k)?
                }
                CommandInstruction::Optional {
                    trigger: t,
                    program: p,
                } => {
                    trigger(t)?;
                    body(p, kind)?;
                    0
                }
                CommandInstruction::Many {
                    trigger: t,
                    program: p,
                }
                | CommandInstruction::Until {
                    trigger: t,
                    program: p,
                } => {
                    trigger(t)?;
                    if body(p, kind)? == 0 {
                        return Err(error(0, "command repetition does not consume input"));
                    }
                    0
                }
                CommandInstruction::Branch {
                    trigger: t,
                    yes,
                    no,
                } => {
                    trigger(t)?;
                    body(yes, kind)?.min(body(no, kind)?)
                }
                CommandInstruction::Choose(rows) => {
                    if rows.is_empty() {
                        return Err(error(0, "empty command choice"));
                    }
                    let mut minimum = usize::MAX;
                    for row in rows {
                        trigger(row.trigger)?;
                        minimum = minimum.min(body(row.program, kind)?);
                    }
                    minimum
                }
                balance @ CommandInstruction::Balance { .. } => {
                    Self::validate_balance(balance, &field, &trigger, &|p| body(p, kind))?
                }
            };
            width = width
                .checked_add(consumed)
                .ok_or_else(|| error(0, "command width overflow"))?;
        }
        Ok(width)
    }
    fn validate_balance(
        instruction: CommandInstruction,
        field: &impl Fn(&'static str) -> Result<(), Diagnostic>,
        trigger: &impl Fn(CommandTrigger) -> Result<(), Diagnostic>,
        body: &impl Fn(&'static [CommandInstruction]) -> Result<usize, Diagnostic>,
    ) -> Result<usize, Diagnostic> {
        let CommandInstruction::Balance {
            open,
            close,
            depth,
            open_field,
            close_field,
            program,
        } = instruction
        else {
            return Err(error(0, "invalid balanced instruction"));
        };
        trigger(open)?;
        trigger(close)?;
        field(open_field)?;
        field(close_field)?;
        if depth == 0 || body(program)? == 0 {
            return Err(error(0, "invalid balanced command body"));
        }
        Ok(1)
    }
    fn validate_leaf(
        &self,
        instruction: CommandInstruction,
        field: &impl Fn(&'static str) -> Result<(), Diagnostic>,
        trigger: &impl Fn(CommandTrigger) -> Result<(), Diagnostic>,
    ) -> Result<usize, Diagnostic> {
        Ok(match instruction {
            CommandInstruction::Raw(f)
            | CommandInstruction::Word(f)
            | CommandInstruction::Command(f) => {
                field(f)?;
                1
            }
            CommandInstruction::Take {
                field: f,
                trigger: t,
            } => {
                field(f)?;
                trigger(t)?;
                1
            }
            CommandInstruction::List {
                field: f,
                until,
                allow_empty,
            } => {
                field(f)?;
                trigger(until)?;
                usize::from(!allow_empty)
            }
            CommandInstruction::Call { field: f, form } => {
                field(f)?;
                if !self.forms.contains_key(form) {
                    return Err(error(0, "unknown command form call"));
                }
                if self.widths.get(form).is_none_or(|width| *width == 0) {
                    return Err(error(0, "command call requires consuming target"));
                }
                1
            }
            _ => return Err(error(0, "invalid command leaf")),
        })
    }
    fn admit_entries(&self, results: &ResultProfileSpec) -> Result<(), Diagnostic> {
        for (id, kind) in [
            (self.spec.assignment_tail, "ArrayAssignment"),
            (self.spec.pipeline_head, "Pipeline"),
        ] {
            let form = self
                .forms
                .get(id)
                .map(|&i| &self.spec.forms[i])
                .ok_or_else(|| error(0, "missing manual entrypoint"))?;
            if !matches!(form.trigger, CommandTrigger::Manual(_)) || form.kind != kind {
                return Err(error(0, "invalid manual entrypoint"));
            }
            let width = self.validate(form.program, kind, results, 0)?;
            if id == self.spec.assignment_tail && width == 0 {
                return Err(error(0, "array body must consume input"));
            }
            if id == self.spec.pipeline_head {
                for step in form.program {
                    let (CommandInstruction::Optional { program: body, .. }
                    | CommandInstruction::Many { program: body, .. }) = step
                    else {
                        return Err(error(0, "pipeline header requires guarded operations"));
                    };
                    if self.validate(body, kind, results, 0)? == 0 {
                        return Err(error(0, "pipeline guard must consume input"));
                    }
                }
            }
        }
        Ok(())
    }
    fn visit(
        &self,
        id: &'static str,
        done: &mut HashSet<&'static str>,
        order: &mut Vec<&'static str>,
    ) -> Result<(), Diagnostic> {
        let mut active = HashSet::new();
        let mut tasks = vec![(id, false)];
        while let Some((id, finish)) = tasks.pop() {
            if finish {
                active.remove(id);
                done.insert(id);
                order.push(id);
                continue;
            }
            if done.contains(id) {
                continue;
            }
            if !active.insert(id) {
                return Err(error(0, "cyclic command form call"));
            }
            tasks.push((id, true));
            let index = *self
                .forms
                .get(id)
                .ok_or_else(|| error(0, "unknown command form call"))?;
            let mut programs = vec![(self.spec.forms[index].program, 0)];
            while let Some((program, depth)) = programs.pop() {
                if depth >= 128 {
                    return Err(error(0, "invalid command program depth"));
                }
                for instruction in program {
                    match instruction {
                        CommandInstruction::Call { form, .. } => tasks.push((form, false)),
                        CommandInstruction::Node { program, .. }
                        | CommandInstruction::Optional { program, .. }
                        | CommandInstruction::Many { program, .. }
                        | CommandInstruction::Until { program, .. }
                        | CommandInstruction::Balance { program, .. } => {
                            programs.push((program, depth + 1));
                        }
                        CommandInstruction::Branch { yes, no, .. } => {
                            programs.push((yes, depth + 1));
                            programs.push((no, depth + 1));
                        }
                        CommandInstruction::Choose(rows) => {
                            programs.extend(rows.iter().map(|r| (r.program, depth + 1)));
                        }
                        _ => {}
                    }
                }
            }
        }
        Ok(())
    }
    pub(super) fn execute<'s>(
        &self,
        id: &str,
        host: &mut impl CommandHost<'s>,
    ) -> Result<ProjectedNode<'s>, Diagnostic> {
        let form = &self.spec.forms[*self
            .forms
            .get(id)
            .ok_or_else(|| error(0, "unknown command entry"))?];
        let start = host.start()?;
        self.execute_initial(form, host, start, Vec::new())
    }
    pub(super) fn execute_initial<'s>(
        &self,
        form: &CommandForm,
        host: &mut impl CommandHost<'s>,
        start: usize,
        children: Vec<ResultChildCapture<'s>>,
    ) -> Result<ProjectedNode<'s>, Diagnostic> {
        let mut frames = vec![Frame {
            kind: form.kind,
            start,
            field: None,
            children,
        }];
        let mut tasks = vec![Task::Finish, Task::Program(form.program, 0)];
        while let Some(task) = tasks.pop() {
            match task {
                Task::Program(program, at) => {
                    let Some(instruction) = program.get(at) else {
                        continue;
                    };
                    tasks.push(Task::Program(program, at + 1));
                    self.step(*instruction, host, &mut frames, &mut tasks)?;
                }
                Task::Finish => {
                    if let Some(node) = self.finish(host, &mut frames, &tasks)? {
                        return Ok(node);
                    }
                }
                Task::Repeat {
                    trigger,
                    program,
                    until,
                    before,
                } => {
                    progress(host.position(), before, host.end())?;
                    if host.matches(trigger)? != until {
                        if !host.available()? {
                            return Err(error(host.end(), "unterminated command repetition"));
                        }
                        tasks.push(Task::Repeat {
                            trigger,
                            program,
                            until,
                            before: Some(host.position()),
                        });
                        tasks.push(Task::Program(program, 0));
                    }
                }
                Task::Balance {
                    open,
                    close,
                    mut depth,
                    open_field,
                    close_field,
                    program,
                    before,
                } => {
                    progress(host.position(), before, host.end())?;
                    if !host.available()? {
                        return Err(error(host.end(), "unterminated balanced command body"));
                    }
                    if host.matches(open)? {
                        publish(&mut frames, open_field, host.raw()?)?;
                        depth = depth
                            .checked_add(1)
                            .ok_or_else(|| error(host.end(), "command depth overflow"))?;
                    } else if host.matches(close)? {
                        publish(&mut frames, close_field, host.raw()?)?;
                        depth -= 1;
                        if depth == 0 {
                            continue;
                        }
                    } else {
                        tasks.push(Task::Balance {
                            open,
                            close,
                            depth,
                            open_field,
                            close_field,
                            program,
                            before: Some(host.position()),
                        });
                        tasks.push(Task::Program(program, 0));
                        continue;
                    }
                    tasks.push(Task::Balance {
                        open,
                        close,
                        depth,
                        open_field,
                        close_field,
                        program,
                        before: None,
                    });
                }
            }
        }
        Err(error(host.end(), "missing command result"))
    }
    fn finish<'s>(
        &self,
        host: &impl CommandHost<'s>,
        frames: &mut Vec<Frame<'s>>,
        tasks: &[Task],
    ) -> Result<Option<ProjectedNode<'s>>, Diagnostic> {
        let frame = frames
            .pop()
            .ok_or_else(|| error(host.end(), "missing command frame"))?;
        let node =
            self.nodes[frame.kind].build(host.source(), frame.start..host.end(), frame.children)?;
        if let Some(field) = frame.field {
            publish(frames, field, ProjectedValue::Node(node))?;
            Ok(None)
        } else if frames.is_empty() && tasks.is_empty() {
            Ok(Some(node))
        } else {
            Err(error(host.end(), "invalid command publication stack"))
        }
    }
    fn step<'s>(
        &self,
        instruction: CommandInstruction,
        host: &mut impl CommandHost<'s>,
        frames: &mut Vec<Frame<'s>>,
        tasks: &mut Vec<Task>,
    ) -> Result<(), Diagnostic> {
        match instruction {
            CommandInstruction::As(kind) => {
                frames
                    .last_mut()
                    .ok_or_else(|| error(host.end(), "missing command frame"))?
                    .kind = kind;
            }
            CommandInstruction::Raw(field) => publish(frames, field, host.raw()?)?,
            CommandInstruction::Word(field) => publish(frames, field, host.word()?)?,
            CommandInstruction::Command(field) => publish(frames, field, host.command()?)?,
            CommandInstruction::Take { field, trigger } => {
                if !host.matches(trigger)? {
                    return Err(error(host.end(), "expected declared command token"));
                }
                publish(frames, field, host.raw()?)?;
            }
            CommandInstruction::List {
                field,
                until,
                allow_empty,
            } => publish(frames, field, host.list(until, allow_empty)?)?,
            CommandInstruction::Call { field, form } => {
                let row = &self.spec.forms[self.forms[form]];
                enter(frames, tasks, host.start()?, field, row.kind, row.program);
            }
            CommandInstruction::Node {
                field,
                kind,
                program,
            } => enter(frames, tasks, host.start()?, field, kind, program),
            CommandInstruction::Optional { trigger, program } => {
                if host.matches(trigger)? {
                    tasks.push(Task::Program(program, 0));
                }
            }
            CommandInstruction::Many { trigger, program } => tasks.push(Task::Repeat {
                trigger,
                program,
                until: false,
                before: None,
            }),
            CommandInstruction::Until { trigger, program } => tasks.push(Task::Repeat {
                trigger,
                program,
                until: true,
                before: None,
            }),
            CommandInstruction::Branch { trigger, yes, no } => tasks.push(Task::Program(
                if host.matches(trigger)? { yes } else { no },
                0,
            )),
            CommandInstruction::Choose(rows) => {
                let mut selected = None;
                for row in rows {
                    if host.matches(row.trigger)? {
                        selected = Some(row.program);
                        break;
                    }
                }
                tasks.push(Task::Program(
                    selected.ok_or_else(|| error(host.end(), "no declared command branch"))?,
                    0,
                ));
            }
            CommandInstruction::Balance {
                open,
                close,
                depth,
                open_field,
                close_field,
                program,
            } => tasks.push(Task::Balance {
                open,
                close,
                depth,
                open_field,
                close_field,
                program,
                before: None,
            }),
        }
        Ok(())
    }
}
fn publish<'s>(
    frames: &mut [Frame<'s>],
    field: &'static str,
    value: ProjectedValue<'s>,
) -> Result<(), Diagnostic> {
    frames
        .last_mut()
        .ok_or_else(|| error(0, "missing command frame"))?
        .children
        .push(ResultChildCapture { field, value });
    Ok(())
}
fn enter(
    frames: &mut Vec<Frame<'_>>,
    tasks: &mut Vec<Task>,
    start: usize,
    field: &'static str,
    kind: &'static str,
    program: &'static [CommandInstruction],
) {
    frames.push(Frame {
        kind,
        start,
        field: Some(field),
        children: Vec::new(),
    });
    tasks.push(Task::Finish);
    tasks.push(Task::Program(program, 0));
}
fn progress(at: usize, before: Option<usize>, byte_offset: usize) -> Result<(), Diagnostic> {
    if before.is_some_and(|before| at <= before) {
        Err(error(byte_offset, "command program did not advance input"))
    } else {
        Ok(())
    }
}

const ROLES: &[&str] = &[
    "trivia",
    "separator",
    "redirect",
    "here-redirect",
    "strip-redirect",
    "reserved",
    "conditional-operator",
    "pipeline",
    "and-or",
    "case-end",
];
fn admit_roles(spec: &CommandProgramSpec, tokens: &HashMap<&str, u16>) -> Result<(), Diagnostic> {
    let mut seen = HashSet::new();
    for role in spec.roles {
        if !ROLES.contains(&role.name) || !seen.insert(role.name) || role.classes.is_empty() {
            return Err(error(0, "invalid command role"));
        }
        let mut kinds = HashSet::new();
        for class in role.classes {
            let mut literals = HashSet::new();
            if !tokens.contains_key(class.kind)
                || !kinds.insert(class.kind)
                || !class
                    .literals
                    .iter()
                    .all(|s| !s.is_empty() && literals.insert(*s))
            {
                return Err(error(0, "invalid role terminal or literal"));
            }
        }
    }
    if seen.len() != ROLES.len() {
        return Err(error(0, "missing command role"));
    }
    Ok(())
}
type Selectors = HashMap<&'static str, HashMap<&'static str, usize>>;
fn admit_selectors(spec: &CommandProgramSpec) -> Result<(Selectors, Vec<usize>), Diagnostic> {
    let mut selectors: Selectors = HashMap::new();
    let mut fallback: Vec<usize> = Vec::new();
    for (index, form) in spec.forms.iter().enumerate() {
        match form.trigger {
            CommandTrigger::Manual(_) => {}
            CommandTrigger::Token {
                kind: "word" | "operator",
                literals,
            } if !literals.is_empty() => {
                let CommandTrigger::Token { kind, .. } = form.trigger else {
                    unreachable!()
                };
                for literal in literals {
                    if selectors
                        .entry(kind)
                        .or_default()
                        .insert(literal, index)
                        .is_some()
                    {
                        return Err(error(0, "ambiguous command form literal"));
                    }
                }
            }
            CommandTrigger::Lookahead(_) | CommandTrigger::Adjacent(_) => {
                if fallback
                    .iter()
                    .any(|&i| spec.forms[i].trigger == form.trigger)
                {
                    return Err(error(0, "duplicate command lookahead selector"));
                }
                fallback.push(index);
            }
            _ => return Err(error(0, "unregistered command entry selector")),
        }
    }
    fallback.reverse();
    Ok((selectors, fallback))
}

const SHAPES: &[(&str, &[&str])] = &[
    ("Redirection", &["descriptor", "operator", "target"]),
    (
        "ArrayAssignment",
        &["assignment", "open", "close", "separator", "element"],
    ),
    (
        "SimpleCommand",
        &["assignment", "name", "argument", "redirect"],
    ),
    ("CommandList", &["command", "separator", "here-document"]),
    ("IfCommand", &["keyword", "condition", "body", "else-body"]),
    ("WhileCommand", &["keyword", "condition", "body"]),
    ("UntilCommand", &["keyword", "condition", "body"]),
    (
        "ForCommand",
        &["keyword", "header", "variable", "item", "separator", "body"],
    ),
    (
        "SelectCommand",
        &["keyword", "header", "variable", "item", "separator", "body"],
    ),
    (
        "ArithmeticForCommand",
        &["keyword", "header", "variable", "item", "separator", "body"],
    ),
    (
        "CaseCommand",
        &["keyword", "subject", "separator", "clause"],
    ),
    (
        "CaseClause",
        &[
            "open",
            "pattern",
            "alternate",
            "close",
            "body",
            "terminator",
        ],
    ),
    (
        "FunctionDefinition",
        &["keyword", "name", "open", "close", "body"],
    ),
    (
        "ConditionalCommand",
        &["open", "close", "operator", "operand"],
    ),
    (
        "ArithmeticCommand",
        &["open", "close", "expression", "operator"],
    ),
    ("RedirectedCommand", &["command", "redirect"]),
    ("BraceGroup", &["open", "body", "close"]),
    ("Subshell", &["open", "body", "close"]),
    (
        "Pipeline",
        &["keyword", "option", "negate", "command", "operator"],
    ),
    ("AndOrList", &["command", "operator"]),
    ("HereDocument", &["delimiter", "content"]),
    ("BashFile", &["command", "separator", "here-document"]),
];
fn admit_shapes(spec: &CommandProgramSpec, results: &ResultProfileSpec) -> Result<(), Diagnostic> {
    if spec.nodes.len() != SHAPES.len() {
        return Err(error(0, "missing or foreign Source projection"));
    }
    for &(role, fields) in SHAPES {
        let name = spec
            .nodes
            .iter()
            .find(|&&(r, _)| r == role)
            .map(|&(_, n)| n)
            .ok_or_else(|| error(0, "missing Source projection"))?;
        let node = results
            .nodes
            .iter()
            .find(|n| results.kinds[usize::from(n.kind)].name == name)
            .ok_or_else(|| error(0, "unknown Source projection"))?;
        if !fields.iter().all(|f| node.fields.contains(f)) {
            return Err(error(0, "Source projection lacks required fields"));
        }
    }
    Ok(())
}
