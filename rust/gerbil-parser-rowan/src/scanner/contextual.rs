//! Execution of the Scheme contextual-scanner-opcodes.v1 data contract.
#[path = "admission.rs"]
mod admission;
#[path = "matching.rs"]
mod matching;
#[path = "obligations.rs"]
mod obligations;
use crate::{Diagnostic, ScannedToken};
use matching::{matcher_end, shell_delimiter};
use obligations::Obligations;
use std::collections::HashMap;
use std::sync::Arc;

pub const SCANNER_OPCODE_CONTRACT: &str = "gerbil-parser.contextual-scanner-opcodes.v1";
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BalancedPair {
    pub prefix: &'static str,
    pub opening: char,
    pub closing: char,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ScannerMatcher {
    Literal(&'static str),
    Literals(&'static [&'static str]),
    HorizontalWhitespace,
    Newline,
    NewlineOne,
    Identifier,
    MarkerLine,
    BodyLine,
    QuotedString(&'static [&'static str]),
    BalancedWord {
        stops: &'static [&'static str],
        quotes: &'static [&'static str],
        pairs: &'static [BalancedPair],
    },
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MarkerPolicy {
    Raw,
    ShellQuoteRemoval,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ScannerAction {
    Keep,
    ExpectMarker(bool),
    EnqueueIfExpecting(MarkerPolicy),
    ActivateNext(&'static str),
    FinishMarker {
        base: &'static str,
        body: &'static str,
    },
}
#[derive(Clone, Copy, Debug)]
pub struct ScannerRule {
    pub name: &'static str,
    pub mode: &'static str,
    pub form: &'static str,
    pub matcher: ScannerMatcher,
    pub rank: i64,
    pub action: ScannerAction,
}
#[derive(Clone, Copy, Debug)]
pub struct ScannerCell {
    pub mode: &'static str,
    pub position: &'static str,
    pub form: &'static str,
    pub terminal: &'static str,
}
#[derive(Debug)]
pub struct ScannerSpec {
    pub opcode_contract: &'static str,
    pub base_grammar_digest: Option<&'static str>,
    pub digest: &'static str,
    pub initial_mode: &'static str,
    pub modes: &'static [&'static str],
    pub positions: &'static [&'static str],
    pub rules: &'static [ScannerRule],
    pub cells: &'static [ScannerCell],
}
#[derive(Clone, Debug, PartialEq, Eq)]
struct Obligation {
    marker: String,
    strip_tabs: bool,
    quoted: bool,
}
#[derive(Debug)]
pub struct ContextualScanner<'source> {
    spec: &'static ScannerSpec,
    source: &'source str,
    rules: HashMap<&'static str, Vec<&'static ScannerRule>>,
    dispatch: HashMap<(&'static str, &'static str), HashMap<&'static str, &'static str>>,
}
/// Immutable checkpoint bound to the prepared scanner and exact source.
#[derive(Clone, Debug)]
pub struct ScannerCheckpoint<'scanner, 'source> {
    owner: &'scanner ContextualScanner<'source>,
    offset: usize,
    mode: &'static str,
    pending: Obligations,
    active: Option<Arc<Obligation>>,
    expecting: Option<bool>,
}
impl ScannerCheckpoint<'_, '_> {
    #[must_use]
    pub fn byte_offset(&self) -> usize {
        self.offset
    }
    #[must_use]
    pub fn mode(&self) -> &'static str {
        self.mode
    }
    #[must_use]
    pub fn pending_markers(&self) -> usize {
        self.pending.len()
    }
    #[must_use]
    pub fn active_marker(&self) -> Option<(&str, bool, bool)> {
        self.active
            .as_ref()
            .map(|o| (o.marker.as_str(), o.strip_tabs, o.quoted))
    }
}
pub(crate) fn error(offset: usize, message: &str) -> Diagnostic {
    Diagnostic {
        reason_kind: "contextual-scanner",
        byte_offset: offset,
        message: message.into(),
    }
}
impl<'source> ContextualScanner<'source> {
    pub(crate) fn source_slice(&self, start: usize, end: usize) -> &'source str {
        &self.source[start..end]
    }
    /// # Errors
    /// Invalid scanner identity, declaration or action target.
    pub fn new(spec: &'static ScannerSpec, source: &'source str) -> Result<Self, Diagnostic> {
        if spec.opcode_contract != SCANNER_OPCODE_CONTRACT
            || !spec.digest.strip_prefix("sha256:").is_some_and(|s| {
                s.len() == 64
                    && s.bytes()
                        .all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b))
            })
            || !spec.modes.contains(&spec.initial_mode)
            || spec.positions.is_empty()
        {
            return Err(error(0, "invalid scanner contract or identity"));
        }
        admission::rules(spec)?;
        for (i, cell) in spec.cells.iter().enumerate() {
            if cell.form.is_empty()
                || cell.terminal.is_empty()
                || !spec.modes.contains(&cell.mode)
                || !spec.positions.contains(&cell.position)
                || spec.cells[..i]
                    .iter()
                    .any(|c| (c.mode, c.position, c.form) == (cell.mode, cell.position, cell.form))
            {
                return Err(error(0, "invalid or duplicate scanner dispatch cell"));
            }
        }
        for rule in spec.rules {
            if !spec.modes.contains(&rule.mode)
                || spec.positions.iter().any(|p| {
                    !spec
                        .cells
                        .iter()
                        .any(|c| c.mode == rule.mode && c.position == *p && c.form == rule.form)
                })
            {
                return Err(error(0, "scanner rule has no dispatch"));
            }
            match rule.action {
                ScannerAction::ActivateNext(mode) if !spec.modes.contains(&mode) => {
                    return Err(error(0, "undeclared action mode"));
                }
                ScannerAction::FinishMarker { base, body }
                    if !spec.modes.contains(&base) || !spec.modes.contains(&body) =>
                {
                    return Err(error(0, "undeclared action mode"));
                }
                _ => {}
            }
        }
        let mut rules: HashMap<_, Vec<_>> = HashMap::new();
        for rule in spec.rules.iter().rev() {
            rules.entry(rule.mode).or_default().push(rule);
        }
        let mut dispatch: HashMap<_, HashMap<_, _>> = HashMap::new();
        for cell in spec.cells {
            dispatch
                .entry((cell.mode, cell.position))
                .or_default()
                .insert(cell.form, cell.terminal);
        }
        Ok(Self {
            spec,
            source,
            rules,
            dispatch,
        })
    }
    #[must_use]
    pub fn initial_state(&self) -> ScannerCheckpoint<'_, 'source> {
        ScannerCheckpoint {
            owner: self,
            offset: 0,
            mode: self.spec.initial_mode,
            pending: Obligations::default(),
            active: None,
            expecting: None,
        }
    }
    /// # Errors
    /// A foreign checkpoint or undeclared mode.
    pub fn with_mode<'scanner>(
        &'scanner self,
        state: &ScannerCheckpoint<'scanner, 'source>,
        mode: &'static str,
    ) -> Result<ScannerCheckpoint<'scanner, 'source>, Diagnostic> {
        self.admit(state, self.spec.positions[0])?;
        if !self.spec.modes.contains(&mode) {
            return Err(error(state.offset, "undeclared scanner mode"));
        }
        let mut next = state.clone();
        next.mode = mode;
        Ok(next)
    }
    fn admit(
        &self,
        state: &ScannerCheckpoint<'_, 'source>,
        position: &str,
    ) -> Result<(), Diagnostic> {
        if !std::ptr::eq(state.owner, self) || !self.spec.positions.contains(&position) {
            return Err(error(
                state.offset,
                "foreign checkpoint or undeclared parser position",
            ));
        }
        Ok(())
    }
    /// # Errors
    /// An ambiguous/unmatched token, foreign checkpoint or unfinished obligation.
    pub fn step<'scanner>(
        &'scanner self,
        state: &ScannerCheckpoint<'scanner, 'source>,
        position: &str,
    ) -> Result<(Option<ScannedToken>, ScannerCheckpoint<'scanner, 'source>), Diagnostic> {
        self.admit(state, position)?;
        if state.offset == self.source.len() {
            if state.active.is_some() || !state.pending.is_empty() || state.expecting.is_some() {
                return Err(error(
                    state.offset,
                    "unfinished contextual scanner obligation",
                ));
            }
            return Ok((None, state.clone()));
        }
        let mut best: Option<(&ScannerRule, usize)> = None;
        // Scheme index-rules reverses declaration order; equivalent ties retain that order.
        for rule in self.rules.get(state.mode).into_iter().flatten() {
            if let Some(end) = matcher_end(
                self.source,
                state.offset,
                rule.matcher,
                state.active.as_deref(),
            )? {
                if end <= state.offset {
                    return Err(error(state.offset, "scanner did not advance"));
                }
                if let Some((current, current_end)) = best {
                    if end < current_end || (end == current_end && rule.rank < current.rank) {
                        continue;
                    }
                    if end == current_end && rule.rank == current.rank {
                        if rule.form != current.form || rule.action != current.action {
                            return Err(error(state.offset, "ambiguous contextual scanner match"));
                        }
                        continue;
                    }
                }
                best = Some((rule, end));
            }
        }
        let (rule, end) =
            best.ok_or_else(|| error(state.offset, "contextual scanner has no match"))?;
        let terminal = self
            .dispatch
            .get(&(state.mode, position))
            .and_then(|forms| forms.get(rule.form))
            .ok_or_else(|| error(state.offset, "missing scanner dispatch"))?;
        let token = ScannedToken {
            terminal,
            start: state.offset,
            end,
        };
        let mut next = state.clone();
        next.offset = end;
        match rule.action {
            ScannerAction::Keep => {}
            ScannerAction::ExpectMarker(strip) => {
                if next.expecting.is_some() {
                    return Err(error(state.offset, "deferred delimiter already expected"));
                }
                next.expecting = Some(strip);
            }
            ScannerAction::EnqueueIfExpecting(policy) => {
                if let Some(strip_tabs) = next.expecting {
                    let word = &self.source[state.offset..end];
                    let (marker, quoted) = match policy {
                        MarkerPolicy::Raw => (word.to_owned(), false),
                        MarkerPolicy::ShellQuoteRemoval => shell_delimiter(word, state.offset)?,
                    };
                    if marker.is_empty() && !quoted {
                        return Err(error(state.offset, "empty deferred delimiter"));
                    }
                    next.pending.push_back(Obligation {
                        marker,
                        strip_tabs,
                        quoted,
                    });
                    next.expecting = None;
                }
            }
            ScannerAction::ActivateNext(body) => {
                if next.expecting.is_some() {
                    return Err(error(state.offset, "missing delimiter before newline"));
                }
                if let Some(active) = next.pending.pop_front() {
                    next.active = Some(active);
                    next.mode = body;
                }
            }
            ScannerAction::FinishMarker { base, body } => {
                if next.active.is_none() {
                    return Err(error(
                        state.offset,
                        "marker closed without active obligation",
                    ));
                }
                next.active = next.pending.pop_front();
                next.mode = if next.active.is_some() { body } else { base };
                next.expecting = None;
            }
        }
        Ok((Some(token), next))
    }
    /// Execute a declaration with one fixed parser position. Dynamic LR positions use step.
    /// # Errors
    /// The first scanner diagnostic; unfinished obligations also fail at EOF.
    pub fn scan(&self, position: &str) -> Result<Vec<ScannedToken>, Diagnostic> {
        let mut state = self.initial_state();
        let mut tokens = Vec::new();
        loop {
            let (token, next) = self.step(&state, position)?;
            state = next;
            if let Some(token) = token {
                tokens.push(token);
            } else {
                return Ok(tokens);
            }
        }
    }
}
