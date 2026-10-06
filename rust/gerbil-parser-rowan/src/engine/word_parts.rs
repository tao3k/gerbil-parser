//! Explicit task-stack composition of declared Word/Binding results.
use super::part_profile::{PartOpcode, PreparedPartProfile, error};
use super::{Diagnostic, ProjectedNode, ResultCapture};
use crate::scanner::PreparedRegionSource;
use std::collections::HashMap;
use std::ops::Range;

enum Slot {
    Span(Option<Range<usize>>),
    Child,
}
enum Task {
    Parts {
        span: Range<usize>,
        context: usize,
    },
    Node {
        span: Range<usize>,
        rule: usize,
    },
    Build {
        projection: usize,
        span: Range<usize>,
        slots: Vec<Slot>,
    },
    Collect(usize),
}
struct WordSource<'a> {
    regions: PreparedRegionSource<'a>,
    subscripts: HashMap<usize, PreparedRegionSource<'a>>,
    text: &'a str,
}
fn check(source: &str, span: &Range<usize>) -> Result<(), Diagnostic> {
    if span.start > span.end || source.get(span.clone()).is_none() {
        Err(error(span.start, "invalid part source span"))
    } else {
        Ok(())
    }
}
fn advance(source: &str, span: Range<usize>, count: usize) -> Result<usize, Diagnostic> {
    let mut at = span.start;
    for ch in source[span.clone()].chars().take(count) {
        at += ch.len_utf8();
    }
    if source[span.start..at].chars().count() == count {
        Ok(at)
    } else {
        Err(error(at, "part opening crosses capture boundary"))
    }
}
fn retreat(source: &str, span: Range<usize>, count: usize) -> Result<usize, Diagnostic> {
    let mut at = span.end;
    for ch in source[span.clone()].chars().rev().take(count) {
        at -= ch.len_utf8();
    }
    if source[at..span.end].chars().count() == count {
        Ok(at)
    } else {
        Err(error(at, "part closing crosses capture boundary"))
    }
}
fn literal(source: &str, span: Range<usize>, vocabulary: &[&str]) -> Option<usize> {
    vocabulary
        .iter()
        .filter(|p| source[span.clone()].starts_with(**p))
        .map(|p| span.start + p.len())
        .max()
}
fn capture(span: Range<usize>) -> Slot {
    Slot::Span((span.start < span.end).then_some(span))
}
fn leaf(projection: usize, span: Range<usize>) -> Task {
    Task::Build {
        projection,
        slots: vec![Slot::Span(Some(span.clone()))],
        span,
    }
}
impl PreparedPartProfile {
    /// Recognize a Word within a bounded slice of the full immutable source.
    /// # Errors
    /// Rejects invalid UTF-8 ranges and malformed or unterminated declared regions.
    pub fn word<'a>(
        &self,
        source: &'a str,
        span: Range<usize>,
    ) -> Result<ProjectedNode<'a>, Diagnostic> {
        self.components(source, span, self.word_context, self.word)
    }
    /// Recognize heredoc content with its declared part context.
    /// # Errors
    /// Rejects invalid UTF-8 ranges and malformed expansions within the capture.
    pub fn here_content<'a>(
        &self,
        source: &'a str,
        span: Range<usize>,
    ) -> Result<ProjectedNode<'a>, Diagnostic> {
        self.components(source, span, self.here_context, self.here)
    }
    /// Recognize an assignment; ordinary words return `None`.
    /// # Errors
    /// Rejects invalid UTF-8 ranges and malformed declared assignment values.
    pub fn assignment<'a>(
        &self,
        source: &'a str,
        span: Range<usize>,
    ) -> Result<Option<ProjectedNode<'a>>, Diagnostic> {
        check(source, &span)?;
        let Some(binding) = self.spec.assignment else {
            return Ok(None);
        };
        let Some(projection) = self.assignment else {
            return Ok(None);
        };
        let Some(name) = self.names[binding].match_prefix(source, span.start, span.end) else {
            return Ok(None);
        };
        let Some(operator) = literal(
            source,
            name..span.end,
            self.spec.bindings[binding].operators,
        ) else {
            return Ok(None);
        };
        let build = Task::Build {
            projection,
            span: span.clone(),
            slots: vec![
                Slot::Span(Some(span.start..name)),
                Slot::Span(Some(name..operator)),
                Slot::Child,
            ],
        };
        self.execute(
            source,
            vec![
                build,
                Task::Parts {
                    span: operator..span.end,
                    context: self.word_context,
                },
            ],
        )
        .map(Some)
    }
    fn components<'a>(
        &self,
        source: &'a str,
        span: Range<usize>,
        context: usize,
        projection: usize,
    ) -> Result<ProjectedNode<'a>, Diagnostic> {
        check(source, &span)?;
        self.execute(
            source,
            vec![
                Task::Build {
                    projection,
                    span: span.clone(),
                    slots: vec![Slot::Child],
                },
                Task::Parts { span, context },
            ],
        )
    }
    fn execute<'a>(
        &self,
        source: &'a str,
        mut tasks: Vec<Task>,
    ) -> Result<ProjectedNode<'a>, Diagnostic> {
        let mut raw = WordSource {
            regions: self.regions.source(source),
            subscripts: HashMap::new(),
            text: source,
        };
        let mut values = Vec::new();
        while let Some(task) = tasks.pop() {
            match task {
                Task::Parts { span, context } => self.parts(&mut raw, span, context, &mut tasks)?,
                Task::Node { span, rule } => self.node(&mut raw, span, rule, &mut tasks)?,
                Task::Collect(count) => {
                    let start = values
                        .len()
                        .checked_sub(count)
                        .ok_or_else(|| error(0, "missing part children"))?;
                    let nodes = values
                        .drain(start..)
                        .map(|value| match value {
                            ResultCapture::Node(node) => Ok(node),
                            _ => Err(error(0, "invalid part child value")),
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    values.push(ResultCapture::Parts(nodes));
                }
                Task::Build {
                    projection,
                    span,
                    slots,
                } => {
                    let count = slots.iter().filter(|s| matches!(s, Slot::Child)).count();
                    let start = values
                        .len()
                        .checked_sub(count)
                        .ok_or_else(|| error(span.start, "missing composite captures"))?;
                    let mut children = values.drain(start..);
                    let captures = slots
                        .into_iter()
                        .map(|slot| match slot {
                            Slot::Span(Some(span)) => Ok(ResultCapture::Span(span)),
                            Slot::Span(None) => Ok(ResultCapture::Absent),
                            Slot::Child => children
                                .next()
                                .ok_or_else(|| error(span.start, "missing child capture")),
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    drop(children);
                    values.push(ResultCapture::Node(
                        self.projections[projection].project(source, span, captures)?,
                    ));
                }
            }
        }
        match values.pop() {
            Some(ResultCapture::Node(node)) if values.is_empty() => Ok(node),
            _ => Err(error(0, "invalid part publication stack")),
        }
    }
    fn parts(
        &self,
        raw: &mut WordSource<'_>,
        span: Range<usize>,
        context: usize,
        tasks: &mut Vec<Task>,
    ) -> Result<(), Diagnostic> {
        let source = raw.text;
        let mut at = span.start;
        let mut children = Vec::new();
        while at < span.end {
            let rule = self.matching(source, at, span.end, context);
            if let Some(index) = rule {
                let rule = &self.spec.rules[index];
                let after = match rule.opcode {
                    PartOpcode::Quote { delimiter } | PartOpcode::QuotedBody { delimiter } => {
                        let quote = advance(source, at..span.end, rule.opening - 1)?;
                        Some(raw.regions.quote_end(quote, delimiter)?)
                    }
                    PartOpcode::Pair | PartOpcode::Parameter { .. } => {
                        Some(raw.regions.pair_end(at)?)
                    }
                    PartOpcode::Name { binding } => self.names[binding].match_prefix(
                        source,
                        advance(source, at..span.end, 1)?,
                        span.end,
                    ),
                    PartOpcode::Escape => Some(
                        source[at..span.end]
                            .chars()
                            .take(2)
                            .fold(at, |end, ch| end + ch.len_utf8()),
                    ),
                };
                if let Some(after) = after {
                    if after <= at || after > span.end {
                        return Err(error(at, "part region crosses capture boundary"));
                    }
                    if matches!(rule.opcode, PartOpcode::Name { .. } | PartOpcode::Escape) {
                        children.push(leaf(self.rule_projections[index], at..after));
                    } else {
                        children.push(Task::Node {
                            span: at..after,
                            rule: index,
                        });
                    }
                    at = after;
                    continue;
                }
            }
            let start = at;
            at = advance(source, at..span.end, 1)?;
            while at < span.end && self.matching(source, at, span.end, context).is_none() {
                at = advance(source, at..span.end, 1)?;
            }
            children.push(leaf(self.literal, start..at));
        }
        tasks.push(Task::Collect(children.len()));
        tasks.extend(children.into_iter().rev());
        Ok(())
    }
    fn node(
        &self,
        raw: &mut WordSource<'_>,
        span: Range<usize>,
        index: usize,
        tasks: &mut Vec<Task>,
    ) -> Result<(), Diagnostic> {
        let rule = &self.spec.rules[index];
        let projection = self.rule_projections[index];
        let start = advance(raw.text, span.clone(), rule.opening)?;
        let end = retreat(raw.text, span.clone(), rule.closing)?;
        if start > end {
            return Err(error(span.start, "overlapping part delimiters"));
        }
        match rule.opcode {
            PartOpcode::Quote { .. } => {
                tasks.push(Task::Build {
                    projection,
                    slots: vec![
                        Slot::Span(Some(span.start..start)),
                        Slot::Child,
                        Slot::Span(Some(end..span.end)),
                    ],
                    span,
                });
                tasks.push(Task::Parts {
                    span: start..end,
                    context: self.quote_contexts[index]
                        .ok_or_else(|| error(start, "missing quote context"))?,
                });
            }
            PartOpcode::Pair | PartOpcode::QuotedBody { .. } => tasks.push(Task::Build {
                projection,
                slots: vec![
                    Slot::Span(Some(span.start..start)),
                    capture(start..end),
                    Slot::Span(Some(end..span.end)),
                ],
                span,
            }),
            PartOpcode::Parameter { binding } => {
                self.parameter(raw, span, start..end, binding, projection, tasks)?;
            }
            _ => return Err(error(span.start, "invalid composite part operation")),
        }
        Ok(())
    }
    fn parameter(
        &self,
        raw: &mut WordSource<'_>,
        span: Range<usize>,
        body: Range<usize>,
        binding: usize,
        projection: usize,
        tasks: &mut Vec<Task>,
    ) -> Result<(), Diagnostic> {
        let source = raw.text;
        let spec = &self.spec.bindings[binding];
        let prefix_limit = source[body.clone()]
            .chars()
            .next_back()
            .map_or(body.start, |ch| body.end - ch.len_utf8());
        let name_start =
            literal(source, body.start..prefix_limit, spec.prefixes).unwrap_or(body.start);
        let name_end = self.names[binding]
            .match_prefix(source, name_start, body.end)
            .unwrap_or(name_start);
        let mut children = Vec::new();
        let (after, subscript) = if let Some(sub) = spec
            .subscript
            .filter(|sub| source[name_end..body.end].starts_with(sub.prefix))
        {
            let plan = self.subscripts[binding]
                .ok_or_else(|| error(name_end, "missing subscript region plan"))?;
            let regions = raw
                .subscripts
                .entry(binding)
                .or_insert_with(|| plan.source(source));
            let after = regions.pair_end(name_end)?;
            if after > body.end {
                return Err(error(
                    name_end,
                    "binding subscript crosses parameter boundary",
                ));
            }
            let inner = name_end + sub.prefix.len();
            let close = retreat(source, inner..after, 1)?;
            let id = self
                .subscript
                .ok_or_else(|| error(name_end, "missing subscript projection"))?;
            children.push(Task::Build {
                projection: id,
                span: name_end..after,
                slots: vec![
                    Slot::Span(Some(name_end..inner)),
                    Slot::Child,
                    Slot::Span(Some(close..after)),
                ],
            });
            children.push(Task::Parts {
                span: inner..close,
                context: self.word_context,
            });
            (after, Slot::Child)
        } else {
            (name_end, Slot::Span(None))
        };
        let operator = literal(source, after..body.end, spec.operators).unwrap_or(after);
        tasks.push(Task::Build {
            projection,
            span: span.clone(),
            slots: vec![
                Slot::Span(Some(span.start..body.start)),
                capture(body.start..name_start),
                capture(name_start..name_end),
                subscript,
                capture(after..operator),
                Slot::Child,
                Slot::Span(Some(body.end..span.end)),
            ],
        });
        tasks.push(Task::Parts {
            span: operator..body.end,
            context: self.word_context,
        });
        // Subscript result precedes the operand capture on the value stack.
        tasks.extend(children);
        Ok(())
    }
}
