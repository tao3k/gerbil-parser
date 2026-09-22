//! Lossless Rowan green-tree emission.

use rowan::{GreenNode, GreenNodeBuilder};

use super::model::{LanguageSpec, Token, Value};

pub(crate) fn build_green(
    spec: &LanguageSpec,
    source: &str,
    tokens: &[Token<'_>],
    root: &Value,
) -> Result<GreenNode, String> {
    let Value::Node { kind, children, .. } = root else {
        return Err("accepted semantic value is not a syntax node".into());
    };
    if *kind != spec.root_kind {
        return Err("accepted syntax node does not have the declared root kind".into());
    }
    let mut builder = GreenNodeBuilder::new();
    let mut next_token = 0;
    builder.start_node(rowan::SyntaxKind(*kind));
    for child in children {
        emit_node(&mut builder, tokens, &child.value, &mut next_token)?;
    }
    while next_token < tokens.len() {
        emit_token(&mut builder, &tokens[next_token]);
        next_token += 1;
    }
    builder.finish_node();
    let green = builder.finish();
    // The lexer covers source monotonically with borrowed slices and emit_node
    // consumes every token exactly once. Length equality therefore proves the
    // lossless roundtrip without allocating a second complete source string.
    if usize::from(green.text_len()) != source.len() {
        return Err("Rowan tree does not cover the complete source".into());
    }
    Ok(green)
}

fn emit_node(
    builder: &mut GreenNodeBuilder<'_>,
    tokens: &[Token<'_>],
    value: &Value,
    next_token: &mut usize,
) -> Result<(), String> {
    match value {
        Value::Token(index) => {
            while *next_token < *index {
                emit_token(builder, &tokens[*next_token]);
                *next_token += 1;
            }
            if *next_token != *index {
                return Err("recognition token order is not monotonic".into());
            }
            emit_token(builder, &tokens[*index]);
            *next_token += 1;
        }
        Value::Node { kind, children, .. } => {
            builder.start_node(rowan::SyntaxKind(*kind));
            for child in children {
                emit_node(builder, tokens, &child.value, next_token)?;
            }
            builder.finish_node();
        }
        Value::Fragment(children) => {
            for child in children {
                emit_node(builder, tokens, &child.value, next_token)?;
            }
        }
    }
    Ok(())
}

fn emit_token(builder: &mut GreenNodeBuilder<'_>, token: &Token<'_>) {
    builder.token(rowan::SyntaxKind(token.syntax_kind), token.text);
}
