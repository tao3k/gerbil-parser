//! Immutable contextual admission and token catalog shared across inputs.
use super::contextual_parser::ContextualParserSpec;
use super::model::{Diagnostic, LexicalRule, TerminalSpec};
use super::validation::{canonical_sha256_digest, validate_spec_once};
use std::collections::HashMap;
use std::sync::{Arc, Mutex, OnceLock};

#[derive(Debug)]
pub(super) struct ContextualPlan {
    pub(super) terminals: HashMap<&'static str, &'static TerminalSpec>,
    pub(super) lexical: HashMap<&'static str, &'static LexicalRule>,
}

pub(super) fn prepare_once(
    product: &'static ContextualParserSpec,
) -> Result<Arc<ContextualPlan>, Diagnostic> {
    static PLANS: OnceLock<Mutex<HashMap<usize, Arc<ContextualPlan>>>> = OnceLock::new();
    let key = std::ptr::from_ref(product) as usize;
    let mut plans = PLANS
        .get_or_init(|| Mutex::new(HashMap::new()))
        .lock()
        .map_err(|_| reject("contextual plan registry poisoned"))?;
    if let Some(plan) = plans.get(&key) {
        return Ok(Arc::clone(plan));
    }
    let spec = product.language;
    validate_spec_once(spec).map_err(|message| reject(&message))?;
    if product.scanner.base_grammar_digest != Some(spec.grammar_digest)
        || !canonical_sha256_digest(product.parser_digest)
        || product.state_positions.len() != spec.actions.len()
        || product
            .state_positions
            .iter()
            .any(|p| !product.scanner.positions.contains(p))
    {
        return Err(reject(
            "invalid contextual parser identity or state positions",
        ));
    }
    // Preserve the existing first-declared lookup semantics for duplicate keys.
    let mut terminals = HashMap::new();
    for terminal in spec.terminals {
        terminals.entry(terminal.name).or_insert(terminal);
    }
    let mut lexical = HashMap::new();
    for rule in spec.lexical_rules {
        lexical.entry(rule.terminal).or_insert(rule);
    }
    let plan = Arc::new(ContextualPlan { terminals, lexical });
    plans.insert(key, Arc::clone(&plan));
    Ok(plan)
}

fn reject(message: &str) -> Diagnostic {
    Diagnostic {
        reason_kind: "contextual-parser",
        byte_offset: 0,
        message: message.into(),
    }
}
