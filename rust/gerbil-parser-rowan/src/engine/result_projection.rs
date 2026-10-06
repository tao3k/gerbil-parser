//! Execution of Scheme-owned result projection tables. Captures move into the
//! tree; nested publication never copies a growing token/event suffix.
use super::model::{Diagnostic, KindCategory, KindSpec, TreeEvent};
use std::collections::{HashMap, HashSet};
use std::ops::Range;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CaptureKind {
    Span,
    Node,
    Parts,
}
#[derive(Clone, Copy, Debug)]
pub struct CaptureSpec {
    pub name: &'static str,
    pub kind: CaptureKind,
}
#[derive(Clone, Copy, Debug)]
pub enum ProjectionOpcode {
    Token { kind: u16, required: bool },
    One { required: bool },
    Many,
}
#[derive(Clone, Copy, Debug)]
pub struct ProjectionInstruction {
    pub field: &'static str,
    pub opcode: ProjectionOpcode,
}
#[derive(Clone, Copy, Debug)]
pub struct ResultProjection {
    pub id: &'static str,
    pub kind: u16,
    pub captures: &'static [CaptureSpec],
    pub instructions: &'static [ProjectionInstruction],
}
#[derive(Clone, Copy, Debug)]
pub struct ResultNodeSpec {
    pub kind: u16,
    pub fields: &'static [&'static str],
}
#[derive(Clone, Copy, Debug)]
pub struct ResultProfileSpec {
    pub kinds: &'static [KindSpec],
    pub nodes: &'static [ResultNodeSpec],
    pub projections: &'static [ResultProjection],
}

pub struct PreparedResultProfile {
    spec: &'static ResultProfileSpec,
    projections: HashMap<&'static str, usize>,
}
/// A projection bound once when a source engine is prepared.
pub struct PreparedResultProjection<'profile> {
    profile: &'profile PreparedResultProfile,
    index: usize,
}
/// A typed recognition capture. `Absent` is admitted only for optional slots.
pub enum ResultCapture<'source> {
    Absent,
    Span(Range<usize>),
    Node(ProjectedNode<'source>),
    Parts(Vec<ProjectedNode<'source>>),
}
/// Source and catalog ownership are private and survive nested composition.
pub struct ProjectedNode<'source> {
    owner: &'static ResultProfileSpec,
    source: &'source str,
    kind: u16,
    span: Range<usize>,
    children: Vec<ProjectedChild<'source>>,
}
pub struct ProjectedChild<'source> {
    field: &'static str,
    value: ProjectedValue<'source>,
}
pub enum ProjectedValue<'source> {
    Token { kind: u16, span: Range<usize> },
    Node(ProjectedNode<'source>),
}
fn error(at: usize, message: &str) -> Diagnostic {
    Diagnostic {
        reason_kind: "invalid-result-projection",
        byte_offset: at,
        message: message.into(),
    }
}
fn unique(values: impl IntoIterator<Item = &'static str>) -> bool {
    let mut seen = HashSet::new();
    values
        .into_iter()
        .all(|name| !name.is_empty() && seen.insert(name))
}
impl PreparedResultProfile {
    /// Admit the entire immutable catalog once, including unused projections.
    /// # Errors
    /// Rejects malformed kinds, fields, capture types or instruction cardinality.
    pub fn new(spec: &'static ResultProfileSpec) -> Result<Self, Diagnostic> {
        if spec.nodes.is_empty()
            || spec.kinds.len() > usize::from(u16::MAX) + 1
            || !unique(
                spec.kinds
                    .iter()
                    .filter(|k| k.category == KindCategory::Node)
                    .map(|k| k.name),
            )
            || !unique(
                spec.kinds
                    .iter()
                    .filter(|k| k.category == KindCategory::Token)
                    .map(|k| k.name),
            )
        {
            return Err(error(0, "invalid result kind catalog"));
        }
        let category = |kind: u16, expected| {
            spec.kinds
                .get(usize::from(kind))
                .is_some_and(|k| k.category == expected)
        };
        let mut nodes = HashMap::new();
        for node in spec.nodes {
            if !category(node.kind, KindCategory::Node)
                || !unique(node.fields.iter().copied())
                || nodes.insert(node.kind, node.fields).is_some()
            {
                return Err(error(0, "invalid result node fields"));
            }
        }
        for (index, kind) in spec.kinds.iter().enumerate() {
            let index = u16::try_from(index).map_err(|_| error(0, "result kind index overflow"))?;
            if kind.category == KindCategory::Node && !nodes.contains_key(&index) {
                return Err(error(0, "missing result node declaration"));
            }
        }
        let mut projections = HashMap::new();
        for (index, projection) in spec.projections.iter().enumerate() {
            let Some(fields) = nodes.get(&projection.kind) else {
                return Err(error(0, "undeclared projection node"));
            };
            if projection.id.is_empty()
                || projections.insert(projection.id, index).is_some()
                || !unique(projection.captures.iter().map(|c| c.name))
                || projection.captures.len() != projection.instructions.len()
            {
                return Err(error(0, "invalid projection identity or arity"));
            }
            for (capture, instruction) in projection.captures.iter().zip(projection.instructions) {
                let valid = match instruction.opcode {
                    ProjectionOpcode::Token { kind, .. } => {
                        capture.kind == CaptureKind::Span && category(kind, KindCategory::Token)
                    }
                    ProjectionOpcode::One { .. } => capture.kind == CaptureKind::Node,
                    ProjectionOpcode::Many => capture.kind == CaptureKind::Parts,
                };
                if !valid || !fields.contains(&instruction.field) {
                    return Err(error(
                        0,
                        "projection instruction disagrees with capture or field",
                    ));
                }
            }
        }
        Ok(Self { spec, projections })
    }
    /// Resolve an instruction entry once during engine preparation.
    /// # Errors
    /// Rejects an undeclared projection identifier.
    pub fn bind(&self, id: &str) -> Result<PreparedResultProjection<'_>, Diagnostic> {
        let index = *self
            .projections
            .get(id)
            .ok_or_else(|| error(0, "unknown result projection"))?;
        Ok(PreparedResultProjection {
            profile: self,
            index,
        })
    }
    /// Bind already recognized captures to the declared result fields.
    /// # Errors
    /// Rejects missing required captures, invalid UTF-8 spans and foreign nodes.
    pub fn project<'source>(
        &self,
        id: &str,
        source: &'source str,
        span: Range<usize>,
        captures: Vec<ResultCapture<'source>>,
    ) -> Result<ProjectedNode<'source>, Diagnostic> {
        self.bind(id)?.project(source, span, captures)
    }
    fn project_at<'source>(
        &self,
        index: usize,
        source: &'source str,
        span: Range<usize>,
        captures: Vec<ResultCapture<'source>>,
    ) -> Result<ProjectedNode<'source>, Diagnostic> {
        let projection = &self.spec.projections[index];
        if captures.len() != projection.captures.len() || !valid_span(source, &span) {
            return Err(error(span.start, "invalid result projection frame"));
        }
        let mut children = Vec::with_capacity(captures.len());
        for (capture, instruction) in captures.into_iter().zip(projection.instructions) {
            let value = match (instruction.opcode, capture) {
                (
                    ProjectionOpcode::Token {
                        required: false, ..
                    }
                    | ProjectionOpcode::One { required: false },
                    ResultCapture::Absent,
                ) => continue,
                (ProjectionOpcode::Token { kind, .. }, ResultCapture::Span(child)) => {
                    if !inside(&child, &span) || !valid_span(source, &child) {
                        return Err(error(span.start, "invalid captured span"));
                    }
                    ProjectedValue::Token { kind, span: child }
                }
                (ProjectionOpcode::One { .. }, ResultCapture::Node(node)) => {
                    self.check_node(source, &span, &node)?;
                    ProjectedValue::Node(node)
                }
                (ProjectionOpcode::Many, ResultCapture::Parts(parts)) => {
                    for node in parts {
                        self.check_node(source, &span, &node)?;
                        children.push(ProjectedChild {
                            field: instruction.field,
                            value: ProjectedValue::Node(node),
                        });
                    }
                    continue;
                }
                _ => return Err(error(span.start, "missing or mistyped result capture")),
            };
            children.push(ProjectedChild {
                field: instruction.field,
                value,
            });
        }
        Ok(ProjectedNode {
            owner: self.spec,
            source,
            kind: projection.kind,
            span,
            children,
        })
    }
    fn check_node(
        &self,
        source: &str,
        span: &Range<usize>,
        node: &ProjectedNode<'_>,
    ) -> Result<(), Diagnostic> {
        if !std::ptr::eq(self.spec, node.owner)
            || !std::ptr::eq(source, node.source)
            || !inside(&node.span, span)
        {
            return Err(error(span.start, "foreign or out-of-frame result node"));
        }
        Ok(())
    }
}
impl PreparedResultProjection<'_> {
    /// Execute bound instructions without another identifier lookup.
    /// # Errors
    /// Rejects invalid captures, source boundaries or node ownership.
    pub fn project<'source>(
        &self,
        source: &'source str,
        span: Range<usize>,
        captures: Vec<ResultCapture<'source>>,
    ) -> Result<ProjectedNode<'source>, Diagnostic> {
        self.profile.project_at(self.index, source, span, captures)
    }
}
fn valid_span(source: &str, span: &Range<usize>) -> bool {
    span.start <= span.end
        && span.end <= source.len()
        && source.is_char_boundary(span.start)
        && source.is_char_boundary(span.end)
}
fn inside(child: &Range<usize>, parent: &Range<usize>) -> bool {
    parent.start <= child.start && child.start <= child.end && child.end <= parent.end
}
impl ProjectedNode<'_> {
    #[must_use]
    pub fn kind(&self) -> u16 {
        self.kind
    }
    #[must_use]
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
    #[must_use]
    pub fn children(&self) -> &[ProjectedChild<'_>] {
        &self.children
    }
    /// Flatten exactly once at the public Rowan handoff. Traversal is iterative.
    #[must_use]
    pub fn events(&self) -> Vec<TreeEvent> {
        enum Visit<'a, 's> {
            Node(&'a ProjectedNode<'s>),
            Value(&'a ProjectedValue<'s>),
            Finish,
        }
        let mut stack = vec![Visit::Node(self)];
        let mut events = Vec::new();
        while let Some(visit) = stack.pop() {
            match visit {
                Visit::Node(node) => {
                    events.push(TreeEvent::StartNode(node.kind));
                    stack.push(Visit::Finish);
                    stack.extend(node.children.iter().rev().map(|c| Visit::Value(&c.value)));
                }
                Visit::Value(ProjectedValue::Node(node)) => stack.push(Visit::Node(node)),
                Visit::Value(ProjectedValue::Token { kind, span }) => {
                    events.push(TreeEvent::Token {
                        kind: *kind,
                        start: span.start,
                        end: span.end,
                    });
                }
                Visit::Finish => events.push(TreeEvent::FinishNode),
            }
        }
        events
    }
}
impl ProjectedChild<'_> {
    #[must_use]
    pub fn field(&self) -> &'static str {
        self.field
    }
    #[must_use]
    pub fn value(&self) -> &ProjectedValue<'_> {
        &self.value
    }
}
