//! Project an existing recognition value into source-range syntax indexes.
use super::event_tree::build_syntax_events_catalog;
use super::model::{EventCatalog, LanguageSpec, Token, TreeEvent, Value};
use gerbil_parser_artifact::syntax::SyntaxTree;
enum Pending<'a> {
    Value(&'a Value),
    Finish,
}
pub(crate) fn build_syntax(
    spec: &LanguageSpec,
    source: &str,
    tokens: &[Token<'_>],
    root: &Value,
) -> Result<SyntaxTree, String> {
    let Value::Node { kind, children, .. } = root else {
        return Err("accepted semantic value is not a syntax node".into());
    };
    if *kind != spec.root_kind {
        return Err("accepted syntax root has an undeclared kind".into());
    }
    let mut events = vec![TreeEvent::StartNode(*kind)];
    let mut pending: Vec<_> = children
        .iter()
        .rev()
        .map(|c| Pending::Value(&c.value))
        .collect();
    let mut next_token = 0;
    while let Some(item) = pending.pop() {
        match item {
            Pending::Finish => events.push(TreeEvent::FinishNode),
            Pending::Value(Value::Node { kind, children, .. }) => {
                events.push(TreeEvent::StartNode(*kind));
                pending.push(Pending::Finish);
                pending.extend(children.iter().rev().map(|c| Pending::Value(&c.value)));
            }
            Pending::Value(Value::Fragment(children)) => {
                pending.extend(children.iter().rev().map(|c| Pending::Value(&c.value)));
            }
            Pending::Value(Value::Token(at)) => {
                if next_token > *at {
                    return Err("recognition token order is not monotonic".into());
                }
                while next_token <= *at {
                    let token = tokens.get(next_token).ok_or("unknown recognition token")?;
                    events.push(TreeEvent::Token {
                        kind: token.syntax_kind,
                        start: token.start,
                        end: token.end,
                    });
                    next_token += 1;
                }
            }
        }
    }
    for token in &tokens[next_token..] {
        events.push(TreeEvent::Token {
            kind: token.syntax_kind,
            start: token.start,
            end: token.end,
        });
    }
    events.push(TreeEvent::FinishNode);
    build_syntax_events_catalog(
        &EventCatalog {
            root_kind: spec.root_kind,
            kinds: spec.kinds,
        },
        source,
        &events,
    )
    .map_err(|d| d.message)
}
