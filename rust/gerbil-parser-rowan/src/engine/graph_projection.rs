//! Linear, language-neutral projection of a validated Rowan CST into records.

use rowan::{NodeOrToken, TextRange, WalkEvent};

use super::model::{Diagnostic, KindCategory, LanguageSpec, SyntaxNode};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct GraphFieldRule {
    pub token_kind: u16,
    pub name: &'static str,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct GraphNodeRule {
    pub syntax_kind: u16,
    pub category: &'static str,
    pub kind: &'static str,
    pub fields: &'static [GraphFieldRule],
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct GraphProjectionSpec {
    pub grammar_digest: &'static str,
    pub projection_digest: &'static str,
    pub rules: &'static [GraphNodeRule],
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GraphRecord {
    pub id: usize,
    pub parent_id: Option<usize>,
    pub child_ids: Vec<usize>,
    pub syntax_kind: u16,
    pub category: &'static str,
    pub kind: &'static str,
    pub range: TextRange,
    pub fields: Vec<GraphFieldValue>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GraphFieldValue {
    pub name: &'static str,
    pub value: String,
}

impl GraphRecord {
    #[must_use]
    pub fn field(&self, name: &str) -> Option<&str> {
        self.fields
            .iter()
            .find(|field| field.name == name)
            .map(|field| field.value.as_str())
    }
}

/// Project one Rowan tree using only AOT-resolved syntax kinds and fields.
///
/// The executor never recognizes language syntax or interprets field values.
/// It walks each node/token once and attaches a token to its nearest projected
/// ancestor. Unprojected CST wrappers are transparent to graph ancestry.
///
/// # Errors
///
/// Returns a diagnostic if the projection table is stale or references a
/// kind outside the generated language catalog.
pub fn project_syntax_graph(
    language: &LanguageSpec,
    spec: &GraphProjectionSpec,
    root: &SyntaxNode,
) -> Result<Vec<GraphRecord>, Diagnostic> {
    let rules = validate_rules(language, spec)?;
    if root.kind().0 != language.root_kind || rules[usize::from(language.root_kind)].is_none() {
        return Err(Diagnostic {
            reason_kind: "invalid-graph-root",
            byte_offset: 0,
            message: "graph projection requires the declared syntax root".into(),
        });
    }
    let mut records = Vec::<GraphRecord>::new();
    let mut projected_stack = Vec::<usize>::new();
    for event in root.preorder_with_tokens() {
        match event {
            WalkEvent::Enter(NodeOrToken::Node(node)) => {
                let kind = usize::from(node.kind().0);
                let Some(rule) = rules.get(kind).and_then(|rule| *rule) else {
                    continue;
                };
                let id = records.len();
                let parent_id = projected_stack.last().copied();
                records.push(GraphRecord {
                    id,
                    parent_id,
                    child_ids: Vec::new(),
                    syntax_kind: rule.syntax_kind,
                    category: rule.category,
                    kind: rule.kind,
                    range: node.text_range(),
                    fields: Vec::with_capacity(rule.fields.len()),
                });
                if let Some(parent) = parent_id {
                    records[parent].child_ids.push(id);
                }
                projected_stack.push(id);
            }
            WalkEvent::Enter(NodeOrToken::Token(token)) => {
                let Some(id) = projected_stack.last().copied() else {
                    continue;
                };
                let Some(rule) = rules[usize::from(records[id].syntax_kind)] else {
                    continue;
                };
                for field in rule.fields {
                    if field.token_kind == token.kind().0 {
                        if let Some(value) = records[id]
                            .fields
                            .iter_mut()
                            .find(|value| value.name == field.name)
                        {
                            value.value.push_str(token.text());
                        } else {
                            records[id].fields.push(GraphFieldValue {
                                name: field.name,
                                value: token.text().to_owned(),
                            });
                        }
                    }
                }
            }
            WalkEvent::Leave(NodeOrToken::Node(node)) => {
                if rules
                    .get(usize::from(node.kind().0))
                    .is_some_and(Option::is_some)
                {
                    projected_stack.pop();
                }
            }
            WalkEvent::Leave(NodeOrToken::Token(_)) => {}
        }
    }
    Ok(records)
}

fn validate_rules<'a>(
    language: &LanguageSpec,
    spec: &'a GraphProjectionSpec,
) -> Result<Vec<Option<&'a GraphNodeRule>>, Diagnostic> {
    let invalid = |message: &'static str| Diagnostic {
        reason_kind: "invalid-graph-aot",
        byte_offset: 0,
        message: message.into(),
    };
    if spec.grammar_digest != language.grammar_digest {
        return Err(invalid(
            "graph and grammar artifacts have different digests",
        ));
    }
    if !super::validation::canonical_sha256_digest(spec.projection_digest) {
        return Err(invalid("graph projection digest is not canonical SHA-256"));
    }
    if language
        .kinds
        .get(usize::from(language.root_kind))
        .is_none()
    {
        return Err(invalid("graph language root kind is unknown"));
    }
    let mut rules = vec![None; language.kinds.len()];
    for rule in spec.rules {
        let Some(kind) = language.kinds.get(usize::from(rule.syntax_kind)) else {
            return Err(invalid("graph rule references an unknown syntax kind"));
        };
        if kind.category != KindCategory::Node || rules[usize::from(rule.syntax_kind)].is_some() {
            return Err(invalid("graph node kind is invalid or declared twice"));
        }
        for field in rule.fields {
            if language
                .kinds
                .get(usize::from(field.token_kind))
                .is_none_or(|kind| kind.category != KindCategory::Token)
                || field.name.is_empty()
            {
                return Err(invalid("graph field references an invalid token or name"));
            }
        }
        rules[usize::from(rule.syntax_kind)] = Some(rule);
    }
    Ok(rules)
}
