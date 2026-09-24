//! Pure list-marker recognition for Scheme-AOT contextual line rules.

use super::model::ListLineRule;

pub(super) struct ListFrame {
    pub(super) indent: usize,
    pub(super) ordered: bool,
}

pub(super) struct ListMarker {
    pub(super) indent: usize,
    pub(super) ordered: bool,
    pub(super) bullet_start: usize,
    pub(super) bullet_end: usize,
    pub(super) content_start: usize,
}

fn leading_indent(line: &str) -> usize {
    line.as_bytes()
        .iter()
        .take_while(|byte| matches!(byte, b' ' | b'\t'))
        .count()
}

pub(super) fn indent_column(line: &str, tab_width: usize) -> usize {
    line.as_bytes()
        .iter()
        .take_while(|byte| matches!(byte, b' ' | b'\t'))
        .fold(0, |column, byte| {
            if *byte == b'\t' {
                (column / tab_width + 1) * tab_width
            } else {
                column + 1
            }
        })
}

pub(super) fn list_marker(line: &str, rule: ListLineRule) -> Option<ListMarker> {
    let bytes = line.as_bytes();
    let bullet_start = leading_indent(line);
    let first = *bytes.get(bullet_start)?;
    let mut bullet_end = bullet_start + 1;
    let ordered = if rule.unordered_markers.as_bytes().contains(&first) {
        false
    } else if rule.ordered && first.is_ascii_alphanumeric() {
        if first.is_ascii_digit() {
            while bytes.get(bullet_end).is_some_and(u8::is_ascii_digit) {
                bullet_end += 1;
            }
        }
        if !matches!(bytes.get(bullet_end), Some(b'.' | b')')) {
            return None;
        }
        bullet_end += 1;
        true
    } else {
        return None;
    };
    if bytes
        .get(bullet_end)
        .is_some_and(|byte| !matches!(byte, b' ' | b'\t' | b'\r' | b'\n'))
    {
        return None;
    }
    let mut content_start = bullet_end;
    while bytes
        .get(content_start)
        .is_some_and(|byte| matches!(byte, b' ' | b'\t'))
    {
        content_start += 1;
    }
    Some(ListMarker {
        indent: indent_column(line, rule.tab_width),
        ordered,
        bullet_start,
        bullet_end,
        content_start,
    })
}
