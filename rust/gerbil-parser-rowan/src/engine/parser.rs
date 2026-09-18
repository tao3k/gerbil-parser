//! Deterministic LR and bounded selective-GLR execution.

use super::lexer::lex;
use super::model::{
    Diagnostic, LanguageSpec, Operand, OperandAction, Parse, ParseError, ParserAction,
    ParserFailure, Production, Reduction, SelectiveGlrReceipt, Terminal, Token, Value,
};
use super::rowan_tree::build_green;
use super::validation::{receipt, validate_spec_once};

/// Parse `source` with one generated language specification.
///
/// # Errors
///
/// Returns [`ParseError`] when the generated artifact is invalid, lexical
/// analysis rejects the source, parsing does not complete, or CST construction
/// fails.
pub fn parse(spec: &'static LanguageSpec, source: &str) -> Result<Parse, ParseError> {
    let receipt = receipt(spec, source);
    validate_spec_once(spec).map_err(|message| ParseError {
        receipt: receipt.clone(),
        diagnostic: Box::new(Diagnostic {
            reason_kind: "invalid-aot-artifact",
            byte_offset: 0,
            message,
        }),
        selective_glr: None,
    })?;
    let (tokens, significant) = lex(spec, source).map_err(|diagnostic| ParseError {
        receipt: receipt.clone(),
        diagnostic: Box::new(diagnostic),
        selective_glr: None,
    })?;
    let (root, selective_glr) =
        parse_tokens(spec, &tokens, &significant).map_err(|failure| ParseError {
            receipt: receipt.clone(),
            diagnostic: Box::new(failure.diagnostic),
            selective_glr: failure.selective_glr,
        })?;
    let green = build_green(spec, source, &tokens, &root).map_err(|message| ParseError {
        receipt: receipt.clone(),
        diagnostic: Box::new(Diagnostic {
            reason_kind: "invalid-cst",
            byte_offset: 0,
            message,
        }),
        selective_glr: None,
    })?;
    Ok(Parse {
        green,
        kinds: spec.kinds,
        receipt,
        selective_glr,
    })
}
const SELECTIVE_GLR_BRANCH_BUDGET: usize = 256;

#[derive(Clone, Debug)]
struct ParserConfiguration {
    states: Vec<u32>,
    values: Vec<Value>,
    cursor: usize,
    score: i32,
}

#[derive(Clone, Debug)]
struct ParseCandidate {
    value: Value,
    cursor: usize,
    score: i32,
    ambiguities: usize,
    winner_reason: &'static str,
}

#[derive(Debug, Default)]
struct GlrContext {
    branches_explored: usize,
    speculative_branches_explored: usize,
    max_speculative_depth: usize,
    merged_branches: usize,
    successful_completions: usize,
    distinct_completions: Vec<Value>,
    best_failure: Option<Diagnostic>,
}

fn parse_tokens(
    spec: &LanguageSpec,
    tokens: &[Token<'_>],
    significant: &[usize],
) -> Result<(Value, SelectiveGlrReceipt), ParserFailure> {
    let mut context = GlrContext::default();
    let candidate = match run_configuration(
        spec,
        tokens,
        significant,
        ParserConfiguration {
            states: vec![0],
            values: Vec::new(),
            cursor: 0,
            score: 0,
        },
        0,
        &mut context,
    ) {
        Ok(candidate) => candidate,
        Err(diagnostic) => {
            let selective_glr = (context.branches_explored > 0)
                .then(|| Box::new(selective_glr_receipt(&context, "no-completion", 0)));
            return Err(ParserFailure {
                diagnostic,
                selective_glr,
            });
        }
    };
    if candidate.ambiguities > 0 {
        return Err(ParserFailure {
            diagnostic: Diagnostic {
                reason_kind: "selective-glr-ambiguity",
                byte_offset: tokens.last().map_or(0, |token| token.end),
                message: format!(
                    "{} equal-score selective-GLR alternatives remain distinct",
                    candidate.ambiguities + 1
                ),
            },
            selective_glr: Some(Box::new(selective_glr_receipt(
                &context,
                "ambiguous",
                candidate.score,
            ))),
        });
    }
    let receipt = selective_glr_receipt(&context, candidate.winner_reason, candidate.score);
    Ok((candidate.value, receipt))
}

fn selective_glr_receipt(
    context: &GlrContext,
    winner_reason: &'static str,
    dynamic_score: i32,
) -> SelectiveGlrReceipt {
    SelectiveGlrReceipt {
        branch_budget: SELECTIVE_GLR_BRANCH_BUDGET,
        branches_explored: context.branches_explored,
        speculative_branches_explored: context.speculative_branches_explored,
        max_speculative_depth: context.max_speculative_depth,
        merged_branches: context.merged_branches,
        successful_completions: context.successful_completions,
        distinct_completions: context.distinct_completions.len(),
        winner_reason,
        dynamic_score,
    }
}

fn run_configuration(
    spec: &LanguageSpec,
    tokens: &[Token<'_>],
    significant: &[usize],
    mut configuration: ParserConfiguration,
    speculative_depth: usize,
    context: &mut GlrContext,
) -> Result<ParseCandidate, Diagnostic> {
    loop {
        let state = *configuration
            .states
            .last()
            .expect("LR state stack is never empty");
        let token = significant
            .get(configuration.cursor)
            .map(|index| &tokens[*index]);
        let Some(action) = find_action(spec, state, token) else {
            let diagnostic = Diagnostic {
                reason_kind: "parse-rejected",
                byte_offset: token
                    .map_or_else(|| tokens.last().map_or(0, |last| last.end), |t| t.start),
                message: format!("no LR action in state {state}"),
            };
            record_failure(context, &diagnostic);
            return Err(diagnostic);
        };
        match action {
            ParserAction::Shift(next) => {
                let Some(&token_index) = significant.get(configuration.cursor) else {
                    return Err(Diagnostic {
                        reason_kind: "invalid-aot-artifact",
                        byte_offset: tokens.last().map_or(0, |token| token.end),
                        message: "generated LR table shifts EOF".into(),
                    });
                };
                configuration.states.push(next);
                configuration.values.push(Value::Token(token_index));
                configuration.cursor += 1;
            }
            ParserAction::Reduce(production_id) => {
                apply_reduce(spec, tokens, token, &mut configuration, production_id)?;
            }
            ParserAction::Accept => {
                if configuration.cursor != significant.len() || configuration.values.len() != 1 {
                    return Err(Diagnostic {
                        reason_kind: "invalid-aot-artifact",
                        byte_offset: token.map_or(0, |token| token.start),
                        message: "generated accept action did not cover one complete root".into(),
                    });
                }
                let value = configuration
                    .values
                    .pop()
                    .expect("one accepted semantic value");
                record_completion(context, &value);
                return Ok(ParseCandidate {
                    value,
                    cursor: configuration.cursor,
                    score: configuration.score,
                    ambiguities: 0,
                    winner_reason: "unique-completion",
                });
            }
            ParserAction::RejectNonAssoc => {
                return Err(Diagnostic {
                    reason_kind: "parse-rejected",
                    byte_offset: token.map_or(0, |token| token.start),
                    message: "non-associative operator chain rejected".into(),
                });
            }
            ParserAction::Fork(branches) => {
                return explore_fork(
                    spec,
                    tokens,
                    significant,
                    &configuration,
                    branches,
                    speculative_depth,
                    context,
                );
            }
        }
    }
}

fn apply_reduce(
    spec: &LanguageSpec,
    tokens: &[Token<'_>],
    token: Option<&Token<'_>>,
    configuration: &mut ParserConfiguration,
    production_id: u32,
) -> Result<(), Diagnostic> {
    let production = spec
        .productions
        .get(production_id as usize)
        .ok_or_else(|| Diagnostic {
            reason_kind: "invalid-aot-artifact",
            byte_offset: token.map_or(0, |token| token.start),
            message: format!("unknown production {production_id}"),
        })?;
    let count = production.rhs.len();
    if configuration.states.len() <= count || configuration.values.len() < count {
        return Err(Diagnostic {
            reason_kind: "invalid-aot-artifact",
            byte_offset: token.map_or(0, |token| token.start),
            message: format!("production {production_id} underflows the LR stack"),
        });
    }
    configuration
        .states
        .truncate(configuration.states.len() - count);
    let reduced_from = configuration.values.len() - count;
    let fallback = token.map_or_else(
        || tokens.last().map_or(0, |last| last.end),
        |lookahead| lookahead.start,
    );
    let reduced = reduce(
        production,
        configuration.values.drain(reduced_from..),
        tokens,
        fallback,
    );
    let owner = *configuration.states.last().expect("LR owner state exists");
    let next = spec.gotos[owner as usize]
        .iter()
        .find(|entry| entry.nonterminal == production.lhs)
        .map(|entry| entry.state)
        .ok_or_else(|| Diagnostic {
            reason_kind: "invalid-aot-artifact",
            byte_offset: fallback,
            message: format!("missing goto for {} from state {owner}", production.lhs),
        })?;
    configuration.states.push(next);
    configuration.values.push(reduced);
    configuration.score += production.dynamic_precedence;
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn explore_fork(
    spec: &LanguageSpec,
    tokens: &[Token<'_>],
    significant: &[usize],
    configuration: &ParserConfiguration,
    branches: &[ParserAction],
    speculative_depth: usize,
    context: &mut GlrContext,
) -> Result<ParseCandidate, Diagnostic> {
    let mut best = None;
    for (index, branch) in branches.iter().enumerate() {
        context.branches_explored += 1;
        let branch_depth = speculative_depth + usize::from(index > 0);
        if index > 0 {
            context.speculative_branches_explored += 1;
            context.max_speculative_depth = context.max_speculative_depth.max(branch_depth);
        }
        if branch_depth > SELECTIVE_GLR_BRANCH_BUDGET {
            return Err(Diagnostic {
                reason_kind: "selective-glr-budget-exhausted",
                byte_offset: current_offset(tokens, significant, configuration.cursor),
                message: format!(
                    "selective-GLR speculative depth exceeds {SELECTIVE_GLR_BRANCH_BUDGET}"
                ),
            });
        }
        match execute_branch_action(
            spec,
            tokens,
            significant,
            configuration.clone(),
            *branch,
            branch_depth,
            context,
        ) {
            Ok(candidate) => best = Some(better_candidate(best, candidate, context)),
            Err(diagnostic)
                if matches!(
                    diagnostic.reason_kind,
                    "invalid-aot-artifact" | "selective-glr-budget-exhausted"
                ) =>
            {
                return Err(diagnostic);
            }
            Err(diagnostic) => record_failure(context, &diagnostic),
        }
    }
    best.ok_or_else(|| {
        context.best_failure.clone().unwrap_or_else(|| Diagnostic {
            reason_kind: "parse-rejected",
            byte_offset: current_offset(tokens, significant, configuration.cursor),
            message: "no selective-GLR branch completed".into(),
        })
    })
}

#[allow(clippy::too_many_arguments)]
fn execute_branch_action(
    spec: &LanguageSpec,
    tokens: &[Token<'_>],
    significant: &[usize],
    mut configuration: ParserConfiguration,
    action: ParserAction,
    speculative_depth: usize,
    context: &mut GlrContext,
) -> Result<ParseCandidate, Diagnostic> {
    let token = significant
        .get(configuration.cursor)
        .map(|index| &tokens[*index]);
    match action {
        ParserAction::Shift(next) => {
            let Some(&token_index) = significant.get(configuration.cursor) else {
                return Err(Diagnostic {
                    reason_kind: "invalid-aot-artifact",
                    byte_offset: tokens.last().map_or(0, |token| token.end),
                    message: "generated LR table shifts EOF".into(),
                });
            };
            configuration.states.push(next);
            configuration.values.push(Value::Token(token_index));
            configuration.cursor += 1;
            run_configuration(
                spec,
                tokens,
                significant,
                configuration,
                speculative_depth,
                context,
            )
        }
        ParserAction::Reduce(production) => {
            apply_reduce(spec, tokens, token, &mut configuration, production)?;
            run_configuration(
                spec,
                tokens,
                significant,
                configuration,
                speculative_depth,
                context,
            )
        }
        ParserAction::Accept => {
            if configuration.cursor != significant.len() || configuration.values.len() != 1 {
                return Err(Diagnostic {
                    reason_kind: "invalid-aot-artifact",
                    byte_offset: current_offset(tokens, significant, configuration.cursor),
                    message: "generated accept action did not cover one complete root".into(),
                });
            }
            let value = configuration
                .values
                .pop()
                .expect("one accepted semantic value");
            record_completion(context, &value);
            Ok(ParseCandidate {
                value,
                cursor: configuration.cursor,
                score: configuration.score,
                ambiguities: 0,
                winner_reason: "unique-completion",
            })
        }
        ParserAction::RejectNonAssoc => Err(Diagnostic {
            reason_kind: "parse-rejected",
            byte_offset: current_offset(tokens, significant, configuration.cursor),
            message: "non-associative operator chain rejected".into(),
        }),
        ParserAction::Fork(branches) => explore_fork(
            spec,
            tokens,
            significant,
            &configuration,
            branches,
            speculative_depth,
            context,
        ),
    }
}

fn better_candidate(
    current: Option<ParseCandidate>,
    candidate: ParseCandidate,
    context: &mut GlrContext,
) -> ParseCandidate {
    let Some(mut current) = current else {
        return candidate;
    };
    if candidate.score > current.score {
        return ParseCandidate {
            winner_reason: "dynamic-precedence",
            ..candidate
        };
    }
    if candidate.score < current.score {
        current.winner_reason = "dynamic-precedence";
        return current;
    }
    if candidate.cursor > current.cursor {
        return ParseCandidate {
            winner_reason: "maximal-consumption",
            ..candidate
        };
    }
    if candidate.cursor < current.cursor {
        current.winner_reason = "maximal-consumption";
        return current;
    }
    if candidate.value == current.value {
        context.merged_branches += 1;
        current.ambiguities += candidate.ambiguities;
        current.winner_reason = if current.ambiguities == 0 {
            "equivalent-merge"
        } else {
            "ambiguous"
        };
        current
    } else {
        current.ambiguities += candidate.ambiguities + 1;
        current.winner_reason = "ambiguous";
        current
    }
}

fn record_completion(context: &mut GlrContext, value: &Value) {
    context.successful_completions += 1;
    if !context
        .distinct_completions
        .iter()
        .any(|completion| completion == value)
    {
        context.distinct_completions.push(value.clone());
    }
}

fn record_failure(context: &mut GlrContext, diagnostic: &Diagnostic) {
    if context
        .best_failure
        .as_ref()
        .is_none_or(|current| diagnostic.byte_offset > current.byte_offset)
    {
        context.best_failure = Some(diagnostic.clone());
    }
}

fn current_offset(tokens: &[Token<'_>], significant: &[usize], cursor: usize) -> usize {
    significant.get(cursor).map_or_else(
        || tokens.last().map_or(0, |token| token.end),
        |index| tokens[*index].start,
    )
}

fn find_action(spec: &LanguageSpec, state: u32, token: Option<&Token<'_>>) -> Option<ParserAction> {
    let row = spec.actions.get(state as usize)?;
    if let Some(token) = token {
        let mut token_action = None;
        for entry in *row {
            match entry.terminal {
                Terminal::Literal(value)
                    if if spec.case_insensitive {
                        value.eq_ignore_ascii_case(token.text)
                    } else {
                        value == token.text
                    } =>
                {
                    return Some(entry.action);
                }
                Terminal::Token(name) if name == token.terminal => {
                    if token_action.is_none() {
                        token_action = Some(entry.action);
                    }
                }
                Terminal::Eof | Terminal::Literal(_) | Terminal::Token(_) => {}
            }
        }
        token_action
    } else {
        row.iter()
            .find(|entry| entry.terminal == Terminal::Eof)
            .map(|entry| entry.action)
    }
}

fn reduce(
    production: &Production,
    mut values: impl ExactSizeIterator<Item = Value>,
    tokens: &[Token<'_>],
    fallback: usize,
) -> Value {
    if production.reduction == Reduction::Pass && values.len() == 1 {
        return apply_operand_actions(
            &production.rhs[0],
            values.next().expect("one pass value"),
            tokens,
            fallback,
        );
    }
    let mut groups = Vec::with_capacity(values.len());
    for (operand, value) in production.rhs.iter().zip(values) {
        groups.push(apply_operand_actions(operand, value, tokens, fallback));
    }
    match production.reduction {
        Reduction::Pass | Reduction::Concat => {
            Value::Fragment(groups.into_iter().flat_map(Value::into_children).collect())
        }
    }
}

fn apply_operand_actions(
    operand: &Operand,
    mut value: Value,
    tokens: &[Token<'_>],
    fallback: usize,
) -> Value {
    for action in operand.actions {
        match *action {
            OperandAction::Field(field) => {
                let mut children = value.into_children();
                for child in &mut children {
                    child.field = Some(field);
                }
                value = Value::Fragment(children);
            }
            OperandAction::Alias(kind) => {
                let start = value.start(tokens, fallback);
                let end = value.end(tokens, fallback);
                value = Value::Node {
                    kind,
                    start,
                    end,
                    children: value.into_children(),
                };
            }
        }
    }
    value
}
