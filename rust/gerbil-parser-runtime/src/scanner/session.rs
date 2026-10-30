//! Owned fixed-position scanning history; no caller-supplied checkpoints.
use super::obligations::Obligations;
use super::{ContextualScanner, Obligation, ScannerCheckpoint, ScannerPlan, ScannerSpec};
use crate::{Diagnostic, ScannedToken};
use std::sync::Arc;

#[derive(Debug)]
struct RecordedState {
    offset: usize,
    mode: &'static str,
    pending: Obligations,
    active: Option<Arc<Obligation>>,
    expecting: Option<bool>,
}
impl RecordedState {
    fn capture(state: &ScannerCheckpoint<'_, '_>) -> Self {
        Self {
            offset: state.offset,
            mode: state.mode,
            pending: state.pending.clone(),
            active: state.active.clone(),
            expecting: state.expecting,
        }
    }
    fn bind<'a, 's>(&self, owner: &'a ContextualScanner<'s>) -> ScannerCheckpoint<'a, 's> {
        ScannerCheckpoint {
            owner,
            offset: self.offset,
            mode: self.mode,
            pending: self.pending.clone(),
            active: self.active.clone(),
            expecting: self.expecting,
        }
    }
}

/// A complete scan owning its source and private token/context history.
/// Only the same immutable plan and fixed parser position can reuse its suffix.
/// Recognition and artifact publication still execute on the complete new source.
#[derive(Debug)]
pub struct ContextualScanSession {
    spec: &'static ScannerSpec,
    plan: Arc<ScannerPlan>,
    source: String,
    position: String,
    tokens: Vec<ScannedToken>,
    states: Vec<RecordedState>,
    scanned: usize,
    reused: usize,
}
impl ContextualScanSession {
    /// Scan new owned input, optionally using an earlier successful scan.
    /// A different plan or position causes a full scan. Failed scans publish no session.
    /// # Errors
    /// Invalid specifications, positions or lexical obligations.
    pub fn new(
        spec: &'static ScannerSpec,
        source: String,
        position: &str,
        previous: Option<&Self>,
    ) -> Result<Self, Diagnostic> {
        let scanner = ContextualScanner::new(spec, &source)?;
        let previous = previous
            .filter(|old| Arc::ptr_eq(&old.plan, &scanner.plan) && old.position == position);
        let old_scanner = previous.map(|old| ContextualScanner {
            spec: old.spec,
            source: &old.source,
            plan: old.plan.clone(),
        });
        let suffix_start = previous.map(|old| common_suffix_start(&old.source, &source));
        let mut state = scanner.initial_state();
        let mut tokens = Vec::new();
        let mut states = vec![RecordedState::capture(&state)];
        let mut executed = 0;
        let mut reused = 0;
        loop {
            if let (Some(old), Some(old_scanner), Some(start)) =
                (previous, old_scanner.as_ref(), suffix_start)
                && state.offset >= start
            {
                let old_offset = old.source.len() - (source.len() - state.offset);
                if let Ok(index) = old.states.binary_search_by_key(&old_offset, |s| s.offset)
                    && state.same_context(&old.states[index].bind(old_scanner))
                {
                    // Common suffix bytes and complete contexts certify this recorded
                    // continuation. Rebind private history, never foreign checkpoints.
                    let relocate = |offset| source.len() - (old.source.len() - offset);
                    tokens.extend(old.tokens[index..].iter().map(|token| ScannedToken {
                        terminal: token.terminal,
                        start: relocate(token.start),
                        end: relocate(token.end),
                    }));
                    states.extend(old.states[index + 1..].iter().map(|record| {
                        let mut rebound = record.bind(&scanner);
                        rebound.offset = relocate(record.offset);
                        RecordedState::capture(&rebound)
                    }));
                    reused = old.tokens.len() - index;
                    break;
                }
            }
            let (token, next) = scanner.step(&state, position)?;
            state = next;
            if let Some(token) = token {
                tokens.push(token);
                states.push(RecordedState::capture(&state));
                executed += 1;
            } else {
                break;
            }
        }
        let plan = scanner.plan.clone();
        Ok(Self {
            spec,
            plan,
            source,
            position: position.to_owned(),
            tokens,
            states,
            scanned: executed,
            reused,
        })
    }
    #[must_use]
    pub fn source(&self) -> &str {
        &self.source
    }
    #[must_use]
    pub fn tokens(&self) -> &[ScannedToken] {
        &self.tokens
    }
    #[must_use]
    pub fn scanned_token_count(&self) -> usize {
        self.scanned
    }
    #[must_use]
    pub fn reused_token_count(&self) -> usize {
        self.reused
    }
    pub(crate) fn admits(&self, spec: &'static ScannerSpec, position: &str) -> bool {
        std::ptr::eq(self.spec, spec) && self.position == position
    }
}

fn common_suffix_start(old: &str, new: &str) -> usize {
    let count = old
        .bytes()
        .rev()
        .zip(new.bytes().rev())
        .take_while(|(a, b)| a == b)
        .count();
    let mut start = new.len() - count;
    // An equal byte suffix can start within a UTF-8 scalar; use complete characters.
    while !new.is_char_boundary(start) || !old.is_char_boundary(old.len() - (new.len() - start)) {
        start += 1;
    }
    start
}
