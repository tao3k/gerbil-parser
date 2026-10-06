//! Closed opcode admission mirrors the Scheme compiler before scanner execution.
use super::{ScannerAction, ScannerMatcher, ScannerRule, ScannerSpec, error};
use crate::Diagnostic;
fn strings(values: &[&str]) -> bool {
    !values.is_empty() && values.iter().all(|v| !v.is_empty())
}
fn matcher(value: ScannerMatcher) -> bool {
    match value {
        ScannerMatcher::Literal(s) => !s.is_empty(),
        ScannerMatcher::Literals(ss) | ScannerMatcher::QuotedString(ss) => strings(ss),
        ScannerMatcher::UnlessPrefix {
            prefixes,
            exceptions,
            child,
        } => {
            strings(prefixes)
                && (exceptions.is_empty() || strings(exceptions))
                && !matches!(child, ScannerMatcher::UnlessPrefix { .. })
                && matcher(*child)
        }
        ScannerMatcher::LinePrefix { prefix, separator } => {
            !prefix.is_empty() && !prefix.contains(separator)
        }
        ScannerMatcher::RegionWord {
            stops,
            quotes,
            pairs,
            ..
        } => {
            strings(stops)
                && stops
                    .iter()
                    .enumerate()
                    .all(|(i, s)| !stops[..i].contains(s))
                && pairs.iter().enumerate().all(|(i, p)| {
                    p.depth > 0
                        && p.depth <= p.prefix.chars().count()
                        && p.prefix.ends_with(p.opening)
                        && !pairs[..i].iter().any(|prior| prior.prefix == p.prefix)
                })
                && quotes.iter().enumerate().all(|(i, q)| {
                    !quotes[..i]
                        .iter()
                        .any(|prior| prior.delimiter == q.delimiter)
                        && q.pairs.iter().enumerate().all(|(j, prefix)| {
                            !q.pairs[..j].contains(prefix)
                                && pairs.iter().any(|p| p.prefix == *prefix)
                        })
                })
        }
        ScannerMatcher::BalancedWord {
            stops,
            quotes,
            pairs,
        } => {
            strings(stops)
                && strings(quotes)
                && quotes
                    .iter()
                    .enumerate()
                    .all(|(i, q)| q.chars().count() == 1 && !quotes[..i].contains(q))
                && pairs.iter().enumerate().all(|(i, p)| {
                    p.prefix.ends_with(p.opening)
                        && !pairs[..i].iter().any(|prior| prior.prefix == p.prefix)
                })
        }
        _ => true,
    }
}
fn action(rule: &ScannerRule) -> bool {
    let matcher = match rule.matcher {
        ScannerMatcher::UnlessPrefix { child, .. } => *child,
        other => other,
    };
    match rule.action {
        ScannerAction::Keep => !matches!(
            matcher,
            ScannerMatcher::MarkerLine | ScannerMatcher::MarkerLineAt(_)
        ),
        ScannerAction::ExpectMarker(_) | ScannerAction::ExpectMarkerIn { .. } => matches!(
            matcher,
            ScannerMatcher::Literal(_) | ScannerMatcher::Literals(_)
        ),
        ScannerAction::EnqueueIfExpecting(_) | ScannerAction::EnqueueMarkerIn { .. } => matches!(
            matcher,
            ScannerMatcher::BalancedWord { .. }
                | ScannerMatcher::RegionWord { .. }
                | ScannerMatcher::Identifier
                | ScannerMatcher::QuotedString(_)
        ),
        ScannerAction::ActivateNext(_) => matches!(
            matcher,
            ScannerMatcher::NewlineOne | ScannerMatcher::Literal(_)
        ),
        ScannerAction::FinishMarker { .. } => matches!(
            matcher,
            ScannerMatcher::MarkerLine | ScannerMatcher::MarkerLineAt(_)
        ),
    }
}
pub(super) fn rules(spec: &ScannerSpec) -> Result<(), Diagnostic> {
    if spec.modes.is_empty()
        || spec
            .modes
            .iter()
            .enumerate()
            .any(|(i, m)| m.is_empty() || spec.modes[..i].contains(m))
        || spec
            .positions
            .iter()
            .enumerate()
            .any(|(i, p)| p.is_empty() || spec.positions[..i].contains(p))
    {
        return Err(error(0, "empty or duplicate scanner modes/positions"));
    }
    for (i, rule) in spec.rules.iter().enumerate() {
        if rule.name.is_empty()
            || rule.form.is_empty()
            || !matcher(rule.matcher)
            || !action(rule)
            || spec.rules[..i].iter().any(|r| r.name == rule.name)
        {
            return Err(error(0, "invalid or duplicate scanner rule"));
        }
    }
    Ok(())
}
