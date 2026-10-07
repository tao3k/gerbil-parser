//! Generic lossless row/cell emission for a declared delimiter-led table.

use super::model::{TableLineRule, TreeEvent};

pub(super) fn is_table_line(line: &str, rule: TableLineRule) -> bool {
    line.trim_start_matches([' ', '\t'])
        .as_bytes()
        .first()
        .is_some_and(|byte| *byte == rule.delimiter)
}

pub(super) fn emit_table_row(
    events: &mut Vec<TreeEvent>,
    rule: TableLineRule,
    line: &str,
    start: usize,
) {
    let bytes = line.as_bytes();
    let indent = bytes
        .iter()
        .take_while(|byte| matches!(byte, b' ' | b'\t'))
        .count();
    let content_end = bytes
        .iter()
        .rposition(|byte| !matches!(byte, b'\r' | b'\n'))
        .map_or(indent, |index| index + 1);
    let delimiter_positions: Vec<_> = (indent..content_end)
        .filter(|&index| bytes[index] == rule.delimiter && !is_escaped(bytes, index))
        .collect();
    if is_rule_row(bytes, indent, content_end, rule.delimiter) {
        events.push(TreeEvent::StartNode(rule.rule_row_node));
        token(events, rule.rule_token, start, start + bytes.len());
        events.push(TreeEvent::FinishNode);
        return;
    }
    events.push(TreeEvent::StartNode(rule.row_node));
    token_nonempty(events, rule.trivia_token, start, start + indent);
    for (index, &position) in delimiter_positions.iter().enumerate() {
        token(
            events,
            rule.separator_token,
            start + position,
            start + position + 1,
        );
        let cell_start = position + 1;
        let cell_end = delimiter_positions
            .get(index + 1)
            .copied()
            .unwrap_or(content_end);
        if index + 1 < delimiter_positions.len()
            || bytes[cell_start..cell_end]
                .iter()
                .any(|byte| !matches!(byte, b' ' | b'\t'))
        {
            events.push(TreeEvent::StartNode(rule.cell_node));
            token_nonempty(
                events,
                rule.cell_token,
                start + cell_start,
                start + cell_end,
            );
            events.push(TreeEvent::FinishNode);
        } else {
            token_nonempty(
                events,
                rule.trivia_token,
                start + cell_start,
                start + cell_end,
            );
        }
    }
    token_nonempty(
        events,
        rule.trivia_token,
        start + content_end,
        start + bytes.len(),
    );
    events.push(TreeEvent::FinishNode);
}

fn is_escaped(bytes: &[u8], index: usize) -> bool {
    let mut prefix = index;
    while prefix > 0 && bytes[prefix - 1] == b'\\' {
        prefix -= 1;
    }
    (index - prefix) % 2 == 1
}

fn is_rule_row(bytes: &[u8], indent: usize, end: usize, delimiter: u8) -> bool {
    let body = &bytes[indent..end];
    body.contains(&b'-')
        && body
            .iter()
            .all(|byte| *byte == delimiter || matches!(byte, b'+' | b'-' | b':' | b' ' | b'\t'))
}

fn token(events: &mut Vec<TreeEvent>, kind: u16, start: usize, end: usize) {
    events.push(TreeEvent::Token { kind, start, end });
}

fn token_nonempty(events: &mut Vec<TreeEvent>, kind: u16, start: usize, end: usize) {
    if start < end {
        token(events, kind, start, end);
    }
}
