//! UTF-8 recognizers for the shared scanner opcode contract.
use super::{BalancedPair, Obligation, RegionPair, RegionQuote, ScannerMatcher, error};
use crate::Diagnostic;
fn next(source: &str, at: usize) -> usize {
    at + source[at..].chars().next().map_or(0, char::len_utf8)
}
fn newline(source: &str, at: usize) -> Option<usize> {
    if source[at..].starts_with("\r\n") {
        Some(at + 2)
    } else if source[at..].starts_with(['\r', '\n']) {
        Some(at + 1)
    } else {
        None
    }
}
fn run(source: &str, at: usize, predicate: impl Fn(char) -> bool) -> Option<usize> {
    let mut end = at;
    for ch in source[at..].chars() {
        if !predicate(ch) {
            break;
        }
        end += ch.len_utf8();
    }
    (end > at).then_some(end)
}
pub(super) fn matcher_end(
    source: &str,
    at: usize,
    matcher: ScannerMatcher,
    active: Option<&Obligation>,
) -> Result<Option<usize>, Diagnostic> {
    let tail = &source[at..];
    Ok(match matcher {
        ScannerMatcher::Literal(s) => tail.starts_with(s).then_some(at + s.len()),
        ScannerMatcher::Literals(ss) => ss
            .iter()
            .filter(|s| tail.starts_with(**s))
            .map(|s| at + s.len())
            .max(),
        ScannerMatcher::HorizontalWhitespace => run(source, at, |c| c == ' ' || c == '\t'),
        ScannerMatcher::Newline => run(source, at, |c| c == '\r' || c == '\n'),
        ScannerMatcher::NewlineOne => newline(source, at),
        ScannerMatcher::Identifier => {
            if tail.starts_with(|c: char| c.is_alphabetic() || c == '_') {
                run(source, at, |c| c.is_alphanumeric() || c == '_' || c == '-')
            } else {
                None
            }
        }
        ScannerMatcher::BodyLine | ScannerMatcher::MarkerLine => active.and_then(|a| {
            let end = tail
                .find(['\r', '\n'])
                .map_or(source.len(), |i| newline(source, at + i).unwrap_or(at + i));
            let content = source[at..end].trim_end_matches(['\r', '\n']);
            if matcher == ScannerMatcher::BodyLine
                || (if a.strip_tabs {
                    content.trim_start_matches('\t')
                } else {
                    content
                }) == a.marker
            {
                Some(end)
            } else {
                None
            }
        }),
        ScannerMatcher::QuotedString(delimiters) => {
            let mut found = None;
            for delimiter in delimiters {
                if tail.starts_with(delimiter) {
                    let mut cursor = at + delimiter.len();
                    while cursor < source.len() {
                        if source[cursor..].starts_with(delimiter) {
                            let end = cursor + delimiter.len();
                            if source[end..].starts_with(delimiter) {
                                cursor = end + delimiter.len();
                                continue;
                            }
                            found = Some(end);
                            break;
                        }
                        if source[cursor..].starts_with('\\') {
                            cursor = next(source, cursor);
                            if cursor < source.len() {
                                cursor = next(source, cursor);
                            }
                        } else {
                            cursor = next(source, cursor);
                        }
                    }
                    if found.is_some() {
                        break;
                    }
                }
            }
            found
        }
        ScannerMatcher::RegionWord {
            stops,
            quotes,
            pairs,
            consume_initial_stop,
        } => region_word(source, at, stops, quotes, pairs, consume_initial_stop)?,
        ScannerMatcher::BalancedWord {
            stops,
            quotes,
            pairs,
        } => balanced_word(source, at, stops, quotes, pairs)?,
    })
}
#[derive(Clone, Copy)]
enum Frame {
    Quote(char),
    Pair(char, char, usize),
}
fn pair_at<'a>(source: &str, at: usize, pairs: &'a [BalancedPair]) -> Option<&'a BalancedPair> {
    pairs
        .iter()
        .filter(|p| source[at..].starts_with(p.prefix))
        .max_by_key(|p| p.prefix.len())
}
fn balanced_word(
    source: &str,
    at: usize,
    stops: &[&str],
    quotes: &[&str],
    pairs: &[BalancedPair],
) -> Result<Option<usize>, Diagnostic> {
    let stopped = |cursor| {
        source[cursor..].starts_with(char::is_whitespace)
            || stops.iter().any(|s| source[cursor..].starts_with(s))
    };
    if stopped(at) {
        return Ok(None);
    }
    let mut stack = Vec::new();
    let mut cursor = at;
    while cursor < source.len() {
        let ch = source[cursor..]
            .chars()
            .next()
            .ok_or_else(|| error(cursor, "missing character"))?;
        let quote = quotes
            .iter()
            .find(|q| source[cursor..].starts_with(**q))
            .and_then(|q| q.chars().next());
        let pair = pair_at(source, cursor, pairs);
        match stack.last().copied() {
            Some(Frame::Quote(delimiter)) => {
                if ch == delimiter {
                    stack.pop();
                    cursor = next(source, cursor);
                    continue;
                }
                if ch == '\\' && delimiter != '\'' {
                    cursor = next(source, cursor);
                    if cursor < source.len() {
                        cursor = next(source, cursor);
                    }
                    continue;
                }
                if delimiter == '"'
                    && let Some(p) = pair
                {
                    stack.push(Frame::Pair(p.opening, p.closing, 1));
                    cursor += p.prefix.len();
                    continue;
                }
            }
            Some(Frame::Pair(open, close, depth)) => {
                if ch == '\\' {
                    cursor = next(source, cursor);
                    if cursor < source.len() {
                        cursor = next(source, cursor);
                    }
                    continue;
                }
                if let Some(q) = quote {
                    stack.push(Frame::Quote(q));
                    cursor = next(source, cursor);
                    continue;
                }
                if let Some(p) = pair {
                    stack.push(Frame::Pair(p.opening, p.closing, 1));
                    cursor += p.prefix.len();
                    continue;
                }
                if ch == open {
                    if let Some(frame) = stack.last_mut() {
                        *frame = Frame::Pair(open, close, depth + 1);
                    }
                } else if ch == close {
                    if depth == 1 {
                        stack.pop();
                    } else if let Some(frame) = stack.last_mut() {
                        *frame = Frame::Pair(open, close, depth - 1);
                    }
                }
            }
            None => {
                if cursor > at && stopped(cursor) {
                    break;
                }
                if ch == '\\' {
                    cursor = next(source, cursor);
                    if cursor < source.len() {
                        cursor = next(source, cursor);
                    }
                    continue;
                }
                if let Some(q) = quote {
                    stack.push(Frame::Quote(q));
                    cursor = next(source, cursor);
                    continue;
                }
                if let Some(p) = pair {
                    stack.push(Frame::Pair(p.opening, p.closing, 1));
                    cursor += p.prefix.len();
                    continue;
                }
            }
        }
        cursor = next(source, cursor);
    }
    if !stack.is_empty() {
        return Err(error(at, "unterminated balanced word quote or pair"));
    }
    Ok(Some(cursor))
}
pub(super) fn shell_delimiter(word: &str, at: usize) -> Result<(String, bool), Diagnostic> {
    let mut chars = word.chars().peekable();
    let mut quote = None;
    let mut quoted = false;
    let mut result = String::new();
    while let Some(ch) = chars.next() {
        if quote != Some('\'') && ch == '\\' {
            if let Some(escaped) = chars.next() {
                if escaped == '\n' {
                    continue;
                }
                if quote == Some('"') && !['$', '`', '"', '\\'].contains(&escaped) {
                    result.push('\\');
                    result.push(escaped);
                } else {
                    quoted = true;
                    result.push(escaped);
                }
            } else {
                quoted = true;
                result.push(ch);
            }
        } else if (quote != Some('"') && ch == '\'') || (quote != Some('\'') && ch == '"') {
            quote = if quote.is_some() { None } else { Some(ch) };
            quoted = true;
        } else {
            result.push(ch);
        }
    }
    if quote.is_some() {
        return Err(error(at, "unterminated delimiter quote"));
    }
    Ok((result, quoted))
}

#[derive(Clone, Copy)]
enum RegionFrame<'a> {
    Quote(&'a RegionQuote),
    Pair(&'a RegionPair, usize),
}
fn region_pair<'a>(
    source: &str,
    at: usize,
    pairs: &'a [RegionPair],
    allowed: Option<&[&str]>,
) -> Option<&'a RegionPair> {
    pairs
        .iter()
        .filter(|p| {
            allowed.is_none_or(|names| names.contains(&p.prefix))
                && source[at..].starts_with(p.prefix)
        })
        .max_by_key(|p| p.prefix.len())
}
fn skip_escape(source: &str, at: usize) -> usize {
    let after = next(source, at);
    if after < source.len() {
        next(source, after)
    } else {
        after
    }
}
fn region_word(
    source: &str,
    at: usize,
    stops: &[&str],
    quotes: &[RegionQuote],
    pairs: &[RegionPair],
    consume_initial_stop: bool,
) -> Result<Option<usize>, Diagnostic> {
    let mut stack = Vec::new();
    let mut cursor = at;
    while cursor < source.len() {
        let ch = source[cursor..]
            .chars()
            .next()
            .ok_or_else(|| error(cursor, "missing region character"))?;
        let frame = stack.last().copied();
        let allowed = match frame {
            Some(RegionFrame::Quote(q)) => Some(q.pairs),
            _ => None,
        };
        let nested = region_pair(source, cursor, pairs, allowed);
        match frame {
            Some(RegionFrame::Quote(q)) => {
                if ch == '\\' && q.escaped {
                    cursor = skip_escape(source, cursor);
                    continue;
                }
                if ch == q.delimiter {
                    stack.pop();
                } else if let Some(p) = nested {
                    stack.push(RegionFrame::Pair(p, p.depth));
                    cursor += p.prefix.len();
                    continue;
                }
            }
            Some(RegionFrame::Pair(p, depth)) => {
                if ch == '\\' {
                    cursor = skip_escape(source, cursor);
                    continue;
                }
                if let Some(q) = quotes.iter().find(|q| q.delimiter == ch) {
                    stack.push(RegionFrame::Quote(q));
                } else if let Some(n) = nested {
                    stack.push(RegionFrame::Pair(n, n.depth));
                    cursor += n.prefix.len();
                    continue;
                } else if ch == p.opening {
                    let depth = depth
                        .checked_add(1)
                        .ok_or_else(|| error(cursor, "region depth overflow"))?;
                    if let Some(frame) = stack.last_mut() {
                        *frame = RegionFrame::Pair(p, depth);
                    }
                } else if ch == p.closing {
                    if depth == 1 {
                        stack.pop();
                    } else if let Some(frame) = stack.last_mut() {
                        *frame = RegionFrame::Pair(p, depth - 1);
                    }
                }
            }
            None => {
                if let Some(p) = nested {
                    stack.push(RegionFrame::Pair(p, p.depth));
                    cursor += p.prefix.len();
                    continue;
                }
                if (cursor > at || !consume_initial_stop)
                    && (ch.is_whitespace() || stops.iter().any(|s| source[cursor..].starts_with(s)))
                {
                    return Ok((cursor > at).then_some(cursor));
                }
                if ch == '\\' {
                    cursor = skip_escape(source, cursor);
                    continue;
                }
                if let Some(q) = quotes.iter().find(|q| q.delimiter == ch) {
                    stack.push(RegionFrame::Quote(q));
                }
            }
        }
        cursor = next(source, cursor);
    }
    if stack.is_empty() {
        Ok((cursor > at).then_some(cursor))
    } else {
        Err(error(at, "unterminated declared region"))
    }
}
