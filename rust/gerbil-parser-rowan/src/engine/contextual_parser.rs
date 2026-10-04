//! Deterministic LR pulls from the shared scanner at the generated state position.
use super::contextual_plan::{ContextualPlan, prepare_once};
use super::model::{
    Diagnostic, LanguageSpec, Parse, ParseError, ParseReceipt, ParserAction, SelectiveGlrReceipt,
    Token, Value,
};
use super::parser::{ParserConfiguration, apply_reduce, find_action};
use super::rowan_tree::build_green;
use super::validation::receipt;
use crate::scanner::{ContextualScanner, ScannerCheckpoint, ScannerSpec};

/// A Scheme-compiled contextual parser binds scanner and LR identities.
#[derive(Debug)]
pub struct ContextualParserSpec {
    pub language: &'static LanguageSpec,
    pub scanner: &'static ScannerSpec,
    pub parser_digest: &'static str,
    pub state_positions: &'static [&'static str],
}
/// Parse with generated LR positions and shared scanner IR.
///
/// # Errors
/// Returns a diagnostic for invalid products, lexical/parse rejection or a GLR
/// fork. Contextual branch-local scanner state is not yet admitted for GLR.
pub fn parse_contextual(
    product: &'static ContextualParserSpec,
    source: &str,
) -> Result<Parse, ParseError> {
    let spec = product.language;
    let bound_receipt = receipt(
        spec,
        source,
        Some(product.parser_digest),
        Some(product.scanner.digest),
    );
    let failure = |diagnostic| ParseError {
        receipt: Box::new(bound_receipt.clone()),
        diagnostic: Box::new(diagnostic),
        selective_glr: None,
    };
    let reject = |offset, message: &str| Diagnostic {
        reason_kind: "contextual-parser",
        byte_offset: offset,
        message: message.into(),
    };
    let plan = prepare_once(product).map_err(&failure)?;
    let scanner = ContextualScanner::new(product.scanner, source).map_err(&failure)?;
    let mut state = scanner.initial_state();
    let mut tokens = Vec::new();
    let mut configuration = ParserConfiguration {
        states: vec![0],
        values: Vec::new(),
        cursor: 0,
        score: 0,
    };
    let mut lookahead = None;
    let mut eof = false;
    loop {
        let lr_state = *configuration
            .states
            .last()
            .ok_or_else(|| failure(reject(state.byte_offset(), "empty LR stack")))?;
        if lookahead.is_none() && !eof {
            let position = product
                .state_positions
                .get(lr_state as usize)
                .ok_or_else(|| failure(reject(state.byte_offset(), "undeclared LR state")))?;
            lookahead =
                pull_token(&scanner, &mut state, position, &plan, &mut tokens).map_err(&failure)?;
            eof = lookahead.is_none();
        }

        let token = lookahead.map(|index| &tokens[index]);
        let action = find_action(spec, lr_state, token).ok_or_else(|| {
            failure(reject(
                token.map_or(source.len(), |t| t.start),
                "no contextual LR action",
            ))
        })?;
        match action {
            ParserAction::Shift(next) => {
                let index = lookahead
                    .take()
                    .ok_or_else(|| failure(reject(source.len(), "LR table shifts EOF")))?;
                configuration.states.push(next);
                configuration.values.push(Value::Token(index));
                configuration.cursor += 1;
            }
            ParserAction::Reduce(production) => {
                apply_reduce(spec, &tokens, token, &mut configuration, production)
                    .map_err(&failure)?;
            }
            ParserAction::Accept => {
                if !eof || lookahead.is_some() || configuration.values.len() != 1 {
                    return Err(failure(reject(
                        state.byte_offset(),
                        "accept did not cover one complete root",
                    )));
                }
                return finish_parse(spec, source, &tokens, configuration, bound_receipt.clone())
                    .map_err(&failure);
            }

            ParserAction::Fork(_) => {
                return Err(failure(reject(
                    state.byte_offset(),
                    "contextual GLR branch scanner state is not admitted",
                )));
            }
            ParserAction::RejectNonAssoc => {
                return Err(failure(reject(
                    state.byte_offset(),
                    "non-associative operator rejected",
                )));
            }
        }
    }
}

fn pull_token<'scanner, 'source>(
    scanner: &'scanner ContextualScanner<'source>,
    state: &mut ScannerCheckpoint<'scanner, 'source>,
    position: &str,
    plan: &ContextualPlan,
    tokens: &mut Vec<Token<'source>>,
) -> Result<Option<usize>, Diagnostic> {
    loop {
        let (lexeme, next) = scanner.step(state, position)?;
        *state = next;
        let Some(lexeme) = lexeme else {
            return Ok(None);
        };
        let terminal = plan.terminals.get(lexeme.terminal).ok_or_else(|| {
            crate::scanner::error(lexeme.start, "scanner terminal is outside language")
        })?;
        let rule = plan.lexical.get(lexeme.terminal).ok_or_else(|| {
            crate::scanner::error(lexeme.start, "scanner terminal has no lexical rule")
        })?;
        let index = tokens.len();
        tokens.push(Token {
            terminal: terminal.name,
            syntax_kind: terminal.syntax_kind,
            text: scanner.source_slice(lexeme.start, lexeme.end),
            start: lexeme.start,
            end: lexeme.end,
        });
        if !rule.extra {
            return Ok(Some(index));
        }
    }
}

fn finish_parse(
    spec: &LanguageSpec,
    source: &str,
    tokens: &[Token<'_>],
    mut configuration: ParserConfiguration,
    bound_receipt: ParseReceipt,
) -> Result<Parse, Diagnostic> {
    let root = configuration
        .values
        .pop()
        .ok_or_else(|| crate::scanner::error(0, "missing root"))?;
    let green = build_green(spec, source, tokens, &root)
        .map_err(|message| crate::scanner::error(0, &message))?;
    Ok(Parse {
        green,
        kinds: spec.kinds,
        receipt: bound_receipt,
        selective_glr: SelectiveGlrReceipt {
            branch_budget: 0,
            branches_explored: 1,
            speculative_branches_explored: 0,
            max_speculative_depth: 0,
            merged_branches: 0,
            successful_completions: 1,
            distinct_completions: 1,
            winner_reason: "contextual-deterministic",
            dynamic_score: configuration.score,
        },
    })
}
