//! Bounded key/value line recognition and source-backed Rowan events.

use super::model::{Diagnostic, KeyLineContext, KeyLineMode, KeyLineRule, KindCategory, TreeEvent};

#[derive(Clone, Copy)]
struct KeySpan {
    start: usize,
    end: usize,
    value_start: usize,
}

fn equals_key(actual: &[u8], expected: &str, case_insensitive: bool) -> bool {
    if case_insensitive {
        actual.eq_ignore_ascii_case(expected.as_bytes())
    } else {
        actual == expected.as_bytes()
    }
}

fn key_at(line: &[u8], start: usize, rule: KeyLineRule) -> Option<KeySpan> {
    let rest = line.get(start..)?;
    let end = if rule.keys.is_empty() {
        start
            + rest
                .iter()
                .take_while(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'_' | b'-'))
                .count()
    } else {
        let key = rule.keys.iter().find(|key| {
            line.get(start..start + key.len())
                .is_some_and(|actual| equals_key(actual, key, rule.case_insensitive))
                && line.get(start + key.len()) == Some(&rule.separator)
        })?;
        start + key.len()
    };
    if end == start || line.get(end) != Some(&rule.separator) {
        return None;
    }
    let value_start = end
        + 1
        + line[end + 1..]
            .iter()
            .take_while(|byte| matches!(byte, b' ' | b'\t'))
            .count();
    Some(KeySpan {
        start,
        end,
        value_start,
    })
}

fn first_key(line: &[u8], rule: KeyLineRule) -> Option<KeySpan> {
    let indent = if rule.indent {
        line.iter()
            .take_while(|byte| matches!(byte, b' ' | b'\t'))
            .count()
    } else {
        0
    };
    let prefix_end = indent + rule.prefix.len();
    let actual = line.get(indent..prefix_end)?;
    if !equals_key(actual, rule.prefix, rule.case_insensitive) {
        return None;
    }
    key_at(line, prefix_end, rule)
}

fn next_key(line: &[u8], start: usize, rule: KeyLineRule) -> Option<KeySpan> {
    if rule.mode == KeyLineMode::Single {
        return None;
    }
    let mut offset = start;
    while offset < line.len() {
        if matches!(line[offset], b' ' | b'\t') {
            let candidate = offset
                + line[offset..]
                    .iter()
                    .take_while(|byte| matches!(byte, b' ' | b'\t'))
                    .count();
            if let Some(key) = key_at(line, candidate, rule) {
                return Some(key);
            }
            offset = candidate;
        } else {
            offset += 1;
        }
    }
    None
}

fn token(events: &mut Vec<TreeEvent>, kind: u16, start: usize, end: usize) {
    if start < end {
        events.push(TreeEvent::Token { kind, start, end });
    }
}

pub(super) fn match_key_line(line: &str, rule: KeyLineRule) -> bool {
    first_key(line.as_bytes(), rule).is_some()
}

pub(super) fn matching_key_line(
    rules: &[KeyLineRule],
    line: &str,
    after_heading: bool,
) -> Option<KeyLineRule> {
    rules.iter().copied().find(|rule| {
        (rule.context == KeyLineContext::Anywhere || after_heading) && match_key_line(line, *rule)
    })
}

pub(super) fn emit_key_line(
    events: &mut Vec<TreeEvent>,
    line: &str,
    start: usize,
    rule: KeyLineRule,
) {
    let bytes = line.as_bytes();
    let mut key = first_key(bytes, rule).expect("matched key line must emit");
    let mut position = 0;
    events.push(TreeEvent::StartNode(rule.node));
    loop {
        token(
            events,
            rule.trivia_token,
            start + position,
            start + key.start,
        );
        token(events, rule.key_token, start + key.start, start + key.end);
        token(
            events,
            rule.trivia_token,
            start + key.end,
            start + key.value_start,
        );
        let next = next_key(bytes, key.value_start, rule);
        let raw_end = next.map_or(bytes.len(), |following| following.start);
        let value_end = bytes[key.value_start..raw_end]
            .iter()
            .rposition(|byte| !byte.is_ascii_whitespace())
            .map_or(key.value_start, |offset| key.value_start + offset + 1);
        token(
            events,
            rule.value_token,
            start + key.value_start,
            start + value_end,
        );
        position = value_end;
        if let Some(following) = next {
            key = following;
        } else {
            token(
                events,
                rule.trivia_token,
                start + position,
                start + bytes.len(),
            );
            break;
        }
    }
    events.push(TreeEvent::FinishNode);
}

pub(super) fn key_line_references(
    rule: KeyLineRule,
) -> Result<[(u16, KindCategory); 4], Diagnostic> {
    let invalid = |message: &'static str| Diagnostic {
        reason_kind: "invalid-structural-aot",
        byte_offset: 0,
        message: message.into(),
    };
    if !rule.prefix.is_ascii()
        || rule.separator.is_ascii_alphanumeric()
        || rule.separator.is_ascii_whitespace()
        || rule.keys.len() > 16
        || rule.keys.is_empty() && rule.prefix.is_empty()
        || rule.mode == KeyLineMode::Repeated && rule.keys.is_empty()
        || rule.keys.iter().any(|key| {
            key.is_empty()
                || !key
                    .bytes()
                    .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'_' | b'-'))
        })
    {
        return Err(invalid("invalid bounded key-line declaration"));
    }
    Ok([
        (rule.node, KindCategory::Node),
        (rule.key_token, KindCategory::Token),
        (rule.value_token, KindCategory::Token),
        (rule.trivia_token, KindCategory::Token),
    ])
}
