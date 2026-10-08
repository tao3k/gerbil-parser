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
    nodes: HashMap<&'static str, u16>,
}
/// A projection bound once when a source engine is prepared.
pub struct PreparedResultProjection {
    spec: &'static ResultProfileSpec,
    index: usize,
}
/// A catalog-bound constructor for nodes whose fields follow source order.
pub struct PreparedResultNode {
    spec: &'static ResultProfileSpec,
    kind: u16,
    fields: &'static [&'static str],
}
/// Ordered typed child; repetitions preserve caller-supplied lexical order.
pub struct ResultChildCapture<'source> {
    pub field: &'static str,
    pub value: ProjectedValue<'source>,
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
// Drain descendants before each node is dropped, bounding native call depth even
// when a successful parse or a partially assembled failure owns a deep tree.
impl Drop for ProjectedNode<'_> {
    fn drop(&mut self) {
        let mut pending = std::mem::take(&mut self.children);
        while let Some(child) = pending.pop() {
            if let ProjectedValue::Node(mut node) = child.value {
                pending.append(&mut node.children);
            }
        }
    }
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
        Ok(Self {
            spec,
            projections,
            nodes: spec
                .nodes
                .iter()
                .map(|n| (spec.kinds[usize::from(n.kind)].name, n.kind))
                .collect(),
        })
    }
    /// Bind a catalog node for ordered command/Source construction.
    /// # Errors
    /// Rejects unknown node identifiers, including names declared only as tokens.
    pub fn bind_node(&self, name: &str) -> Result<PreparedResultNode, Diagnostic> {
        let kind = *self
            .nodes
            .get(name)
            .ok_or_else(|| error(0, "unknown result node"))?;
        let fields = self
            .spec
            .nodes
            .iter()
            .find(|n| n.kind == kind)
            .ok_or_else(|| error(0, "missing result node fields"))?
            .fields;
        Ok(PreparedResultNode {
            spec: self.spec,
            kind,
            fields,
        })
    }
    /// Resolve an instruction entry once during engine preparation.
    /// # Errors
    /// Rejects an undeclared projection identifier.
    pub fn bind(&self, id: &str) -> Result<PreparedResultProjection, Diagnostic> {
        let index = *self
            .projections
            .get(id)
            .ok_or_else(|| error(0, "unknown result projection"))?;
        Ok(PreparedResultProjection {
            spec: self.spec,
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
        spec: &'static ResultProfileSpec,
        index: usize,
        source: &'source str,
        span: Range<usize>,
        captures: Vec<ResultCapture<'source>>,
    ) -> Result<ProjectedNode<'source>, Diagnostic> {
        let projection = &spec.projections[index];
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
                    Self::check_node(spec, source, &span, &node)?;
                    ProjectedValue::Node(node)
                }
                (ProjectionOpcode::Many, ResultCapture::Parts(parts)) => {
                    for node in parts {
                        Self::check_node(spec, source, &span, &node)?;
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
            owner: spec,
            source,
            kind: projection.kind,
            span,
            children,
        })
    }
    fn check_node(
        spec: &'static ResultProfileSpec,
        source: &str,
        span: &Range<usize>,
        node: &ProjectedNode<'_>,
    ) -> Result<(), Diagnostic> {
        if !std::ptr::eq(spec, node.owner)
            || !std::ptr::eq(source, node.source)
            || !inside(&node.span, span)
        {
            return Err(error(span.start, "foreign or out-of-frame result node"));
        }
        Ok(())
    }
}
impl PreparedResultProjection {
    pub(super) fn has_signature(&self, expected: &[(&str, CaptureKind, bool)]) -> bool {
        let p = &self.spec.projections[self.index];
        p.captures.len() == expected.len()
            && p.captures.iter().zip(p.instructions).zip(expected).all(
                |((capture, instruction), (name, kind, required))| {
                    let actual_required = match instruction.opcode {
                        ProjectionOpcode::Token { required, .. }
                        | ProjectionOpcode::One { required } => required,
                        ProjectionOpcode::Many => false,
                    };
                    capture.name == *name && capture.kind == *kind && actual_required == *required
                },
            )
    }

    /// Execute bound instructions without another identifier lookup.
    /// # Errors
    /// Rejects invalid captures, source boundaries or node ownership.
    pub fn project<'source>(
        &self,
        source: &'source str,
        span: Range<usize>,
        captures: Vec<ResultCapture<'source>>,
    ) -> Result<ProjectedNode<'source>, Diagnostic> {
        PreparedResultProfile::project_at(self.spec, self.index, source, span, captures)
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
impl PreparedResultNode {
    /// Move ordered captures into the admitted node without sorting or cloning.
    /// # Errors
    /// Rejects foreign fields, catalogs, sources, token kinds and invalid UTF-8 ranges.
    pub fn build<'source>(
        &self,
        source: &'source str,
        span: Range<usize>,
        captures: Vec<ResultChildCapture<'source>>,
    ) -> Result<ProjectedNode<'source>, Diagnostic> {
        if !valid_span(source, &span) {
            return Err(error(span.start, "invalid ordered result frame"));
        }
        let mut children = Vec::with_capacity(captures.len());
        for capture in captures {
            if !self.fields.contains(&capture.field) {
                return Err(error(span.start, "undeclared ordered result field"));
            }
            match &capture.value {
                ProjectedValue::Node(node) => {
                    PreparedResultProfile::check_node(self.spec, source, &span, node)?;
                }
                ProjectedValue::Token { kind, span: child } => {
                    if !self
                        .spec
                        .kinds
                        .get(usize::from(*kind))
                        .is_some_and(|k| k.category == KindCategory::Token)
                        || !valid_span(source, child)
                        || !inside(child, &span)
                    {
                        return Err(error(span.start, "invalid ordered result token"));
                    }
                }
            }
            children.push(ProjectedChild {
                field: capture.field,
                value: capture.value,
            });
        }
        Ok(ProjectedNode {
            owner: self.spec,
            source,
            kind: self.kind,
            span,
            children,
        })
    }
}
impl<'source> ProjectedNode<'source> {
    #[must_use]
    pub(super) fn into_captures(mut self) -> Vec<ResultChildCapture<'source>> {
        std::mem::take(&mut self.children)
            .into_iter()
            .map(|child| ResultChildCapture {
                field: child.field,
                value: child.value,
            })
            .collect()
    }
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
    /// Flatten exactly once at the public source-index handoff. Traversal is iterative.
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
