//! Canonical command Source event publication and borrowed result ownership.
use super::super::{Diagnostic, ProjectedNode, ProjectedValue, ResultProfileSpec, TreeEvent};

pub(super) fn error(at: usize, message: &str) -> Diagnostic {
    Diagnostic {
        reason_kind: "invalid-command-source",
        byte_offset: at,
        message: message.into(),
    }
}
/// Recognized Source and FIFO deferred-body links. Fields retain declaration order.
pub struct CommandParse<'s> {
    pub root: ProjectedNode<'s>,
    pub here_documents: Vec<(usize, usize)>,
    pub(super) trivia: Vec<(u16, std::ops::Range<usize>)>,
    pub(super) source: &'s str,
    pub(super) result_owner: &'static ResultProfileSpec,
}
/// Canonical Source publication, retaining fields and producer-order identifiers.
/// Kind indexes refer to the admitted result profile; fields are declaration names.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CommandEvent {
    StartNode {
        id: u64,
        kind: u16,
        offset: usize,
    },
    FinishNode {
        id: u64,
        kind: u16,
        offset: usize,
    },
    StartField {
        name: &'static str,
        offset: usize,
    },
    FinishField {
        name: &'static str,
        offset: usize,
    },
    Token {
        id: u64,
        kind: u16,
        start: usize,
        end: usize,
    },
}
enum CommandWalk<'a> {
    Node(&'a ProjectedNode<'a>),
    Value(&'a ProjectedValue<'a>, &'static str),
    FinishNode(u64, u16, usize),
    FinishField(&'static str, usize),
}
impl CommandWalk<'_> {
    fn boundary(&self) -> usize {
        match self {
            Self::Node(n) => n.span().start,
            Self::Value(ProjectedValue::Node(n), _) => n.span().start,
            Self::Value(ProjectedValue::Token { span, .. }, _) => span.start,
            Self::FinishNode(_, _, end) | Self::FinishField(_, end) => *end,
        }
    }
}
impl CommandParse<'_> {
    /// Exact borrowed source admitted with this request's token tape.
    #[must_use]
    pub fn source(&self) -> &str {
        self.source
    }

    /// Walk canonical records once, without building an intermediate event tape.
    /// Trivia precedes a child's field scope; trailing trivia belongs to its node.
    /// # Errors
    /// Rejects overlapping captures or trivia outside the root. A sink may have
    /// received a prefix on failure, so callers must commit publication on success.
    pub fn walk_events(&self, mut emit: impl FnMut(CommandEvent)) -> Result<(), Diagnostic> {
        if !self.root.bound_to(self.result_owner, self.source)
            || self.root.span() != (0..self.source.len())
        {
            return Err(error(0, "foreign command Source root"));
        }
        let mut tasks = vec![CommandWalk::Node(&self.root)];
        let mut trivia = self.trivia.iter().peekable();
        let (mut offset, mut node_id, mut token_id) = (0, 0, 0);
        while let Some(task) = tasks.pop() {
            // Closing fields never claim trivia beyond the value's end.
            let boundary = task.boundary();
            while let Some((kind, span)) = trivia.peek() {
                if span.end > boundary {
                    break;
                }
                if span.start != offset {
                    return Err(error(offset, "source trivia does not cover gap"));
                }
                emit(CommandEvent::Token {
                    id: token_id,
                    kind: *kind,
                    start: span.start,
                    end: span.end,
                });
                token_id += 1;
                offset = span.end;
                trivia.next();
            }
            match task {
                CommandWalk::Node(node) => {
                    if node.span().start != offset {
                        return Err(error(offset, "command node is outside source order"));
                    }
                    let id = node_id;
                    node_id += 1;
                    emit(CommandEvent::StartNode {
                        id,
                        kind: node.kind(),
                        offset,
                    });
                    tasks.push(CommandWalk::FinishNode(id, node.kind(), node.span().end));
                    tasks.extend(
                        node.children()
                            .iter()
                            .rev()
                            .map(|c| CommandWalk::Value(c.value(), c.field())),
                    );
                }
                CommandWalk::Value(value, field) => {
                    let span = match value {
                        ProjectedValue::Node(node) => node.span(),
                        ProjectedValue::Token { span, .. } => span.clone(),
                    };
                    if span.start != offset {
                        return Err(error(offset, "command value is outside source order"));
                    }
                    emit(CommandEvent::StartField {
                        name: field,
                        offset,
                    });
                    tasks.push(CommandWalk::FinishField(field, span.end));
                    match value {
                        ProjectedValue::Node(node) => tasks.push(CommandWalk::Node(node)),
                        ProjectedValue::Token { kind, span } => {
                            emit(CommandEvent::Token {
                                id: token_id,
                                kind: *kind,
                                start: span.start,
                                end: span.end,
                            });
                            token_id += 1;
                            offset = span.end;
                        }
                    }
                }
                CommandWalk::FinishNode(id, kind, end) => {
                    if end != offset {
                        return Err(error(offset, "unclaimed significant source range"));
                    }
                    emit(CommandEvent::FinishNode { id, kind, offset });
                }
                CommandWalk::FinishField(name, end) => {
                    if end != offset {
                        return Err(error(offset, "field is outside source order"));
                    }
                    emit(CommandEvent::FinishField { name, offset });
                }
            }
        }
        if trivia.next().is_some() {
            return Err(error(offset, "trivia remains outside source"));
        }
        Ok(())
    }
    /// Publish the lossless CST tape from the same canonical event walk.
    /// # Errors
    /// Rejects overlapping captures or trivia outside the root.
    pub fn events(&self) -> Result<Vec<TreeEvent>, Diagnostic> {
        let mut events = Vec::new();
        self.walk_events(|event| match event {
            CommandEvent::StartNode { kind, .. } => events.push(TreeEvent::StartNode(kind)),
            CommandEvent::FinishNode { .. } => events.push(TreeEvent::FinishNode),
            CommandEvent::Token {
                kind, start, end, ..
            } => events.push(TreeEvent::Token { kind, start, end }),
            CommandEvent::StartField { .. } | CommandEvent::FinishField { .. } => {}
        })?;
        Ok(events)
    }
}
