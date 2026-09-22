//! AOT artifact validation and parse-receipt identity.

use std::cell::RefCell;
use std::collections::HashSet;

use sha2::{Digest, Sha256};

use super::model::{
    KindCategory, LanguageSpec, LexicalExpr, OperandAction, ParseReceipt, ParserAction,
};

pub(crate) fn receipt(spec: &LanguageSpec, source: &str) -> ParseReceipt {
    const HEX: &[u8; 16] = b"0123456789abcdef";
    let digest = Sha256::digest(source.as_bytes());
    let mut encoded = String::with_capacity(71);
    encoded.push_str("sha256:");
    for byte in digest {
        encoded.push(char::from(HEX[usize::from(byte >> 4)]));
        encoded.push(char::from(HEX[usize::from(byte & 0x0f)]));
    }
    ParseReceipt {
        language: spec.language,
        version: spec.version,
        contract: spec.contract,
        grammar_digest: spec.grammar_digest,
        source_digest: encoded,
    }
}

fn validate_spec(spec: &LanguageSpec) -> Result<(), String> {
    if spec.grammar_digest.len() != 71 || !spec.grammar_digest.starts_with("sha256:") {
        return Err("grammar digest is not a canonical SHA-256 identity".into());
    }
    if spec.kinds.len() > usize::from(u16::MAX) + 1 {
        return Err("syntax-kind catalog exceeds Rowan u16 identity space".into());
    }
    if usize::from(spec.root_kind) >= spec.kinds.len()
        || spec.kinds[usize::from(spec.root_kind)].category != KindCategory::Node
    {
        return Err("root kind is not a declared node".into());
    }
    if spec.actions.len() != spec.gotos.len() {
        return Err("LR action and goto state counts differ".into());
    }
    for terminal in spec.terminals {
        let Some(kind) = spec.kinds.get(usize::from(terminal.syntax_kind)) else {
            return Err(format!(
                "terminal {} has an unknown syntax kind",
                terminal.name
            ));
        };
        if kind.category != KindCategory::Token {
            return Err(format!(
                "terminal {} does not resolve to a token kind",
                terminal.name
            ));
        }
    }
    for rule in spec.lexical_rules {
        if !spec
            .terminals
            .iter()
            .any(|terminal| terminal.name == rule.terminal)
        {
            return Err(format!(
                "lexical rule references unknown terminal {}",
                rule.terminal
            ));
        }
        validate_lexical_expression(&rule.expression)?;
    }
    let state_count = spec.actions.len();
    for row in spec.actions {
        for entry in *row {
            validate_parser_action(&entry.action, state_count, spec.productions.len())?;
        }
    }
    for row in spec.gotos {
        for entry in *row {
            if entry.state as usize >= state_count {
                return Err(format!("goto targets unknown state {}", entry.state));
            }
        }
    }
    for production in spec.productions {
        for operand in production.rhs {
            for action in operand.actions {
                if let OperandAction::Alias(kind) = action {
                    let Some(specification) = spec.kinds.get(usize::from(*kind)) else {
                        return Err(format!("alias references unknown syntax kind {kind}"));
                    };
                    if specification.category != KindCategory::Node {
                        return Err(format!("alias references non-node syntax kind {kind}"));
                    }
                }
            }
        }
    }
    Ok(())
}

pub(crate) fn validate_spec_once(spec: &'static LanguageSpec) -> Result<(), String> {
    // Public parsing accepts only static generated products. Their addresses
    // are stable for the process lifetime. A thread-local cache keeps the hot
    // path lock-free while paying validation at most once per DSL and parser
    // thread.
    thread_local! {
        static VALIDATED: RefCell<HashSet<usize>> = RefCell::new(HashSet::new());
    }
    let identity = std::ptr::from_ref(spec) as usize;
    if VALIDATED.with_borrow(|validated| validated.contains(&identity)) {
        return Ok(());
    }
    validate_spec(spec)?;
    VALIDATED.with_borrow_mut(|validated| validated.insert(identity));
    Ok(())
}

fn validate_parser_action(
    action: &ParserAction,
    state_count: usize,
    production_count: usize,
) -> Result<(), String> {
    match action {
        ParserAction::Shift(state) if *state as usize >= state_count => {
            Err(format!("shift targets unknown state {state}"))
        }
        ParserAction::Reduce(production) if *production as usize >= production_count => {
            Err(format!("reduce references unknown production {production}"))
        }
        ParserAction::Fork([]) => Err("selective-GLR fork has no branches".into()),
        ParserAction::Fork(branches) => {
            for branch in *branches {
                validate_parser_action(branch, state_count, production_count)?;
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn validate_lexical_expression(expression: &LexicalExpr) -> Result<(), String> {
    fn nonempty(values: &[&str]) -> bool {
        values.iter().all(|value| !value.is_empty())
    }

    match expression {
        LexicalExpr::NumberLiteral {
            prefixes,
            separator,
            suffixes,
            ..
        } if separator.chars().count() != 1 || !nonempty(prefixes) || !nonempty(suffixes) => {
            Err("profiled number contains an invalid separator, prefix, or suffix".into())
        }
        LexicalExpr::QuotedString(delimiters) if delimiters.is_empty() || !nonempty(delimiters) => {
            Err("quoted string requires non-empty delimiters".into())
        }
        LexicalExpr::LineComment(prefixes) if prefixes.is_empty() || !nonempty(prefixes) => {
            Err("line comment requires non-empty prefixes".into())
        }
        LexicalExpr::BlockComment { opening, closing }
        | LexicalExpr::NestedBlockComment { opening, closing }
            if opening.is_empty() || closing.is_empty() =>
        {
            Err("block comment requires non-empty delimiters".into())
        }
        LexicalExpr::Choice([]) => Err("lexical choice requires at least one alternative".into()),
        LexicalExpr::Choice(expressions) => {
            for alternative in *expressions {
                validate_lexical_expression(alternative)?;
            }
            Ok(())
        }
        LexicalExpr::Literals(values) if values.is_empty() || !nonempty(values) => {
            Err("literal expression requires non-empty spellings".into())
        }
        _ => Ok(()),
    }
}
