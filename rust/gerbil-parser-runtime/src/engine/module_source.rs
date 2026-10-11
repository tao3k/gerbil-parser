//! Source-owned module framing indexes. Border runs remain compact intervals.
use super::model::ModuleTextProfile;

struct HeaderInterval {
    first: usize,
    last: usize,
    end: usize,
}
struct DepthEvent {
    at: usize,
    depth: i64,
}
pub(super) struct PreparedModuleSource<'source> {
    source: &'source str,
    profile: &'static ModuleTextProfile,
    headers: Vec<HeaderInterval>,
    events: Vec<DepthEvent>,
}
fn prefix_at(source: &str, at: usize, text: &str) -> bool {
    source
        .get(at..)
        .is_some_and(|suffix| suffix.starts_with(text))
}
fn run_end(source: &str, start: usize, accept: impl Fn(char) -> bool) -> usize {
    let mut end = start;
    for ch in source[start..].chars() {
        if !accept(ch) {
            break;
        }
        end += ch.len_utf8();
    }
    end
}
fn whitespace(ch: char) -> bool {
    super::unicode_whitespace::is_scheme_whitespace(ch)
}
fn name_end(source: &str, at: usize, extra: char) -> Option<usize> {
    let mut end = at;
    let mut seen = false;
    for ch in source[at..].chars() {
        let numeric = super::unicode_numeric::is_scheme_numeric(ch);
        let alphabetic = super::unicode_alphabetic::is_scheme_alphabetic(ch);
        if !numeric && !alphabetic && ch != extra {
            break;
        }
        seen |= alphabetic || ch == extra;
        end += ch.len_utf8();
    }
    seen.then_some(end)
}
fn header_tail_end(source: &str, at: usize, profile: &ModuleTextProfile) -> Option<usize> {
    let word = run_end(source, at, whitespace);
    if !prefix_at(source, word, profile.header_word) {
        return None;
    }
    let name = run_end(source, word + profile.header_word.len(), whitespace);
    let name_stop = name_end(source, name, profile.name_extra.chars().next()?)?;
    let close = run_end(source, name_stop, whitespace);
    if !prefix_at(source, close, profile.header_border) {
        return None;
    }
    let border = profile.header_border.chars().next()?;
    Some(run_end(source, close, |ch| ch == border))
}
fn header_intervals(source: &str, profile: &ModuleTextProfile) -> Vec<HeaderInterval> {
    let border = profile.header_border.chars().next().unwrap();
    let mut rows = Vec::new();
    let mut at = 0;
    while at < source.len() {
        let ch = source[at..].chars().next().unwrap();
        if ch == border {
            let end = run_end(source, at, |ch| ch == border);
            if end - at >= profile.header_border.len()
                && let Some(stop) = header_tail_end(source, end, profile)
            {
                rows.push(HeaderInterval {
                    first: at,
                    last: end - profile.header_border.len(),
                    end: stop,
                });
            }
            at = end;
        } else {
            at += ch.len_utf8();
        }
    }
    rows
}
fn skip_block(source: &str, start: usize, open: &str, close: &str) -> usize {
    let mut at = start + open.len();
    let mut depth = 1;
    while at < source.len() {
        if prefix_at(source, at, open) {
            depth += 1;
            at += open.len();
        } else if prefix_at(source, at, close) {
            at += close.len();
            depth -= 1;
            if depth == 0 {
                break;
            }
        } else {
            at += source[at..].chars().next().unwrap().len_utf8();
        }
    }
    at
}
fn skip_string(source: &str, start: usize) -> usize {
    let mut at = start + 1;
    while at < source.len() {
        let ch = source[at..].chars().next().unwrap();
        at += ch.len_utf8();
        if ch == '"' {
            break;
        }
        if ch == '\\'
            && let Some(next) = source[at..].chars().next()
        {
            at += next.len_utf8();
        }
    }
    at
}
fn depth_events(
    source: &str,
    profile: &ModuleTextProfile,
    headers: &[HeaderInterval],
) -> Vec<DepthEvent> {
    let mut events = Vec::new();
    let mut at = 0;
    let mut cursor = 0;
    let mut depth = 0;
    while at < source.len() {
        while headers.get(cursor).is_some_and(|row| row.last < at) {
            cursor += 1;
        }
        if prefix_at(source, at, profile.block_open) {
            at = skip_block(source, at, profile.block_open, profile.block_close);
        } else if source[at..].starts_with('"') {
            at = skip_string(source, at);
        } else if prefix_at(source, at, profile.line_comment) {
            at = run_end(source, at, |ch| ch != '\n');
        } else if let Some(row) = headers.get(cursor).filter(|row| row.first <= at) {
            depth += 1;
            events.push(DepthEvent { at, depth });
            at = row.end;
        } else if prefix_at(source, at, profile.end_border) {
            depth -= 1;
            events.push(DepthEvent { at, depth });
            let ch = profile.end_border.chars().next().unwrap();
            at = run_end(source, at, |item| item == ch);
        } else {
            at += source[at..].chars().next().unwrap().len_utf8();
        }
    }
    events
}
impl<'source> PreparedModuleSource<'source> {
    pub(super) fn new(source: &'source str, profile: &'static ModuleTextProfile) -> Self {
        let headers = header_intervals(source, profile);
        let events = depth_events(source, profile, &headers);
        Self {
            source,
            profile,
            headers,
            events,
        }
    }
    pub(super) fn end(&self, start: usize) -> Option<usize> {
        if start >= self.source.len() || !self.source.is_char_boundary(start) {
            return None;
        }
        if start != 0 {
            let prior = self.source[..start]
                .char_indices()
                .rev()
                .find(|&(_, ch)| !whitespace(ch))?;
            let prior_end = prior.0 + prior.1.len_utf8();
            if !self.source[..prior_end].ends_with(self.profile.end_border) {
                return None;
            }
            let index = self.events.partition_point(|row| row.at < start);
            if index > 0 && self.events[index - 1].depth != 0 {
                return None;
            }
        }
        let index = self.headers.partition_point(|row| row.last < start);
        let end = self
            .headers
            .get(index)
            .map_or(self.source.len(), |row| row.first.max(start));
        (end > start).then_some(end)
    }
}

#[cfg(test)]
#[path = "../../tests/unit/prepared_module.rs"]
mod tests;
