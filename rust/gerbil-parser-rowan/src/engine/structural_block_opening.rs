//! Generic opening recognition for Scheme-declared structural blocks.

use super::model::{BlockLineRule, BlockOpeningMode};

pub(super) fn directive(
    line: &str,
    value: &str,
    case_insensitive: bool,
    indent: bool,
    closing: bool,
) -> bool {
    let bytes = if indent {
        line.trim_start_matches([' ', '\t']).as_bytes()
    } else {
        line.as_bytes()
    };
    let Some(prefix) = bytes.get(..value.len()) else {
        return false;
    };
    let matches = if case_insensitive {
        prefix.eq_ignore_ascii_case(value.as_bytes())
    } else {
        prefix == value.as_bytes()
    };
    if !matches {
        return false;
    }
    let tail = &bytes[value.len()..];
    if closing {
        tail.iter()
            .all(|byte| matches!(byte, b' ' | b'\t' | b'\r' | b'\n'))
    } else {
        tail.first()
            .is_none_or(|byte| matches!(byte, b' ' | b'\t' | b'\r' | b'\n'))
    }
}

pub(super) fn block_opening(line: &str, rule: &BlockLineRule) -> bool {
    match rule.opening_mode {
        BlockOpeningMode::Literal => directive(
            line,
            rule.opening,
            rule.case_insensitive,
            rule.indent,
            false,
        ),
        BlockOpeningMode::NamedDelimited => named_delimited_opening(line, rule).is_some(),
    }
}

pub(super) fn named_delimited_opening(line: &str, rule: &BlockLineRule) -> Option<(usize, usize)> {
    let marker = *rule.opening.as_bytes().first()?;
    let bytes = line.as_bytes();
    let indent = if rule.indent {
        bytes
            .iter()
            .take_while(|byte| matches!(**byte, b' ' | b'\t'))
            .count()
    } else {
        0
    };
    if bytes.get(indent) != Some(&marker)
        || directive(line, rule.closing, rule.case_insensitive, rule.indent, true)
    {
        return None;
    }
    let name_start = indent + 1;
    let name_end = name_start
        + bytes[name_start..]
            .iter()
            .take_while(|byte| byte.is_ascii_alphanumeric() || matches!(**byte, b'_' | b'-'))
            .count();
    if !bytes.get(name_start)?.is_ascii_alphabetic()
        || bytes.get(name_end) != Some(&marker)
        || !bytes[name_end + 1..]
            .iter()
            .all(|byte| matches!(byte, b' ' | b'\t' | b'\r' | b'\n'))
    {
        return None;
    }
    Some((name_start, name_end))
}
