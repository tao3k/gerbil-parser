//! Shared source-local region execution, including nested boundary memoization.
use super::{RegionPair, RegionQuote, error};
use crate::Diagnostic;
use std::collections::HashMap;

/// A pair scope restricts nested pair prefixes; an omitted scope permits all pairs.
#[derive(Clone, Copy, Debug)]
pub struct RegionScope {
    pub prefix: &'static str,
    pub pairs: &'static [&'static str],
}
/// Immutable delimiters, quote nesting and initial word-stop policy lowered by Scheme.
#[derive(Clone, Copy, Debug)]
pub struct RegionSpec {
    pub stops: &'static [&'static str],
    pub quotes: &'static [RegionQuote],
    pub pairs: &'static [RegionPair],
    pub consume_initial_stop: bool,
}
/// Immutable admitted region declarations, reusable across independent sources.
#[derive(Clone, Copy)]
pub struct PreparedRegionPlan {
    spec: &'static RegionSpec,
    scopes: &'static [RegionScope],
}
impl PreparedRegionPlan {
    /// Admit a static region plan once.
    /// # Errors
    /// Rejects malformed declarations and unknown or duplicate scope owners.
    pub fn new(
        spec: &'static RegionSpec,
        scopes: &'static [RegionScope],
    ) -> Result<Self, Diagnostic> {
        if !valid_spec(spec.stops, spec.quotes, spec.pairs)
            || !scopes.iter().enumerate().all(|(i, s)| {
                spec.pairs.iter().any(|p| p.prefix == s.prefix)
                    && !scopes[..i].iter().any(|p| p.prefix == s.prefix)
                    && s.pairs
                        .iter()
                        .all(|n| spec.pairs.iter().any(|p| p.prefix == *n))
            })
        {
            return Err(error(0, "invalid declared region specification or scopes"));
        }
        Ok(Self { spec, scopes })
    }
    /// Borrow another full source with independent lazy boundary caches.
    #[must_use]
    pub fn source(self, source: &str) -> PreparedRegionSource<'_> {
        PreparedRegionSource {
            spec: self.spec,
            source,
            scopes: self.scopes,
            ends: Ends {
                enabled: true,
                ..Ends::default()
            },
        }
    }
}
/// Borrowed full source with lazy caches. Construction never interprets source text.
pub struct PreparedRegionSource<'a> {
    spec: &'static RegionSpec,
    source: &'a str,
    scopes: &'static [RegionScope],
    ends: Ends,
}
#[derive(Default)]
struct Ends {
    enabled: bool,
    #[cfg(test)]
    visits: usize,
    quotes: HashMap<usize, usize>,
    pairs: HashMap<(&'static str, usize), usize>,
}
#[derive(Clone, Copy)]
enum Frame<'a> {
    Quote(&'a RegionQuote, usize),
    Pair(&'a RegionPair, usize, usize),
}
pub(super) fn valid_spec(stops: &[&str], quotes: &[RegionQuote], pairs: &[RegionPair]) -> bool {
    !stops.is_empty()
        && stops
            .iter()
            .enumerate()
            .all(|(i, s)| !s.is_empty() && !stops[..i].contains(s))
        && pairs.iter().enumerate().all(|(i, p)| {
            p.depth > 0
                && p.depth <= p.prefix.chars().count()
                && p.prefix.ends_with(p.opening)
                && !pairs[..i].iter().any(|q| q.prefix == p.prefix)
        })
        && quotes.iter().enumerate().all(|(i, q)| {
            !quotes[..i].iter().any(|r| r.delimiter == q.delimiter)
                && q.pairs
                    .iter()
                    .enumerate()
                    .all(|(j, n)| !q.pairs[..j].contains(n) && pairs.iter().any(|p| p.prefix == *n))
        })
}
impl<'a> PreparedRegionSource<'a> {
    /// Prepare an admitted static plan and lazy source-local caches.
    /// # Errors
    /// Rejects malformed declarations and unknown or duplicate scope owners.
    pub fn new(
        spec: &'static RegionSpec,
        source: &'a str,
        scopes: &'static [RegionScope],
    ) -> Result<Self, Diagnostic> {
        Ok(PreparedRegionPlan::new(spec, scopes)?.source(source))
    }

    fn entry(&self, at: usize) -> Result<(), Diagnostic> {
        if at >= self.source.len() || !self.source.is_char_boundary(at) {
            Err(error(at, "invalid declared region entry"))
        } else {
            Ok(())
        }
    }
    /// Recognize a word starting at an absolute UTF-8 byte offset.
    /// # Errors
    /// Rejects invalid offsets and unterminated nested regions.
    pub fn word_end(&mut self, at: usize) -> Result<Option<usize>, Diagnostic> {
        self.entry(at)?;
        self.scan(at, at, Vec::new(), true)
    }
    /// Return the exclusive byte end of a declared quote.
    /// # Errors
    /// Rejects invalid offsets, mismatched delimiters and unterminated regions.
    pub fn quote_end(&mut self, at: usize, delimiter: char) -> Result<usize, Diagnostic> {
        self.entry(at)?;
        let q = self
            .spec
            .quotes
            .iter()
            .find(|q| q.delimiter == delimiter)
            .filter(|_| self.source[at..].starts_with(delimiter))
            .ok_or_else(|| error(at, "invalid declared quote entry"))?;
        if let Some(end) = self.ends.quotes.get(&at) {
            return Ok(*end);
        }
        self.scan(
            at,
            at + delimiter.len_utf8(),
            vec![Frame::Quote(q, at)],
            false,
        )?
        .ok_or_else(|| error(at, "missing declared quote end"))
    }
    /// Return the exclusive byte end of the longest declared opener.
    /// # Errors
    /// Rejects invalid offsets, missing openers and unterminated regions.
    pub fn pair_end(&mut self, at: usize) -> Result<usize, Diagnostic> {
        self.entry(at)?;
        let p = pair_at(self.source, at, self.spec.pairs, None)
            .ok_or_else(|| error(at, "missing declared region opener"))?;
        if let Some(end) = self.ends.pairs.get(&(p.prefix, at)) {
            return Ok(*end);
        }
        self.scan(
            at,
            at + p.prefix.len(),
            vec![Frame::Pair(p, p.depth, at)],
            false,
        )?
        .ok_or_else(|| error(at, "missing declared region end"))
    }
    fn scan(
        &mut self,
        start: usize,
        at: usize,
        stack: Vec<Frame<'static>>,
        word: bool,
    ) -> Result<Option<usize>, Diagnostic> {
        scan(
            self.source,
            start,
            at,
            stack,
            word,
            self.spec.stops,
            self.spec.quotes,
            self.spec.pairs,
            self.spec.consume_initial_stop,
            self.scopes,
            &mut self.ends,
        )
    }
}
fn next(source: &str, at: usize) -> usize {
    at + source[at..].chars().next().map_or(0, char::len_utf8)
}
fn escape(source: &str, at: usize) -> usize {
    let after = next(source, at);
    if after < source.len() {
        next(source, after)
    } else {
        after
    }
}
fn pair_at<'a>(
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
fn close(frame: Frame<'_>, end: usize, ends: &mut Ends) {
    if !ends.enabled {
        return;
    }
    match frame {
        Frame::Quote(_, start) => {
            ends.quotes.insert(start, end);
        }
        Frame::Pair(p, _, start) => {
            ends.pairs.insert((p.prefix, start), end);
        }
    }
}
// One explicit stack is shared by scanner words and lazy quote/pair decomposition.
#[allow(clippy::too_many_arguments)]
fn scan<'a>(
    source: &str,
    start: usize,
    mut at: usize,
    mut stack: Vec<Frame<'a>>,
    word: bool,
    stops: &[&str],
    quotes: &'a [RegionQuote],
    pairs: &'a [RegionPair],
    consume_initial_stop: bool,
    scopes: &[RegionScope],
    ends: &mut Ends,
) -> Result<Option<usize>, Diagnostic> {
    while at < source.len() {
        #[cfg(test)]
        {
            ends.visits += 1;
        }
        let ch = source[at..]
            .chars()
            .next()
            .ok_or_else(|| error(at, "missing region character"))?;
        let frame = stack.last().copied();
        let allowed = match frame {
            Some(Frame::Quote(q, _)) => Some(q.pairs),
            Some(Frame::Pair(p, _, _)) => scopes
                .iter()
                .find(|s| s.prefix == p.prefix)
                .map(|s| s.pairs),
            None => None,
        };
        let nested = pair_at(source, at, pairs, allowed);
        if let Some(frame) = frame {
            let quote = match frame {
                Frame::Quote(q, _) => Some(q),
                Frame::Pair(..) => None,
            };
            if ch == '\\' && quote.is_none_or(|q| q.escaped) {
                at = escape(source, at);
                continue;
            }
            if quote.is_some_and(|q| q.delimiter == ch) {
                close(frame, next(source, at), ends);
                stack.pop();
            } else if quote.is_none() && quotes.iter().any(|q| q.delimiter == ch) {
                let q = quotes
                    .iter()
                    .find(|q| q.delimiter == ch)
                    .expect("declared quote");
                stack.push(Frame::Quote(q, at));
            } else if let Some(p) = nested {
                if let Some(end) = ends.pairs.get(&(p.prefix, at)) {
                    at = *end;
                    continue;
                }
                stack.push(Frame::Pair(p, p.depth, at));
                at += p.prefix.len();
                continue;
            } else if let Frame::Pair(p, depth, origin) = frame {
                if ch == p.opening {
                    *stack.last_mut().expect("active frame") = Frame::Pair(
                        p,
                        depth
                            .checked_add(1)
                            .ok_or_else(|| error(at, "region depth overflow"))?,
                        origin,
                    );
                } else if ch == p.closing {
                    if depth == 1 {
                        close(frame, next(source, at), ends);
                        stack.pop();
                    } else {
                        *stack.last_mut().expect("active frame") =
                            Frame::Pair(p, depth - 1, origin);
                    }
                }
            }
            at = next(source, at);
            if stack.is_empty() && !word {
                return Ok(Some(at));
            }
        } else {
            if !word {
                return Ok(Some(at));
            }
            if let Some(p) = nested {
                stack.push(Frame::Pair(p, p.depth, at));
                at += p.prefix.len();
                continue;
            }
            if (at > start || !consume_initial_stop)
                && (crate::engine::is_scheme_whitespace(ch)
                    || stops.iter().any(|s| source[at..].starts_with(s)))
            {
                return Ok((at > start).then_some(at));
            }
            if ch == '\\' {
                at = escape(source, at);
                continue;
            }
            if let Some(q) = quotes.iter().find(|q| q.delimiter == ch) {
                stack.push(Frame::Quote(q, at));
            }
            at = next(source, at);
        }
    }
    if stack.is_empty() {
        Ok((at > start).then_some(at))
    } else {
        Err(error(start, "unterminated declared region"))
    }
}
pub(super) fn scan_word(
    source: &str,
    at: usize,
    stops: &[&str],
    quotes: &[RegionQuote],
    pairs: &[RegionPair],
    consume_initial_stop: bool,
) -> Result<Option<usize>, Diagnostic> {
    scan(
        source,
        at,
        at,
        Vec::new(),
        true,
        stops,
        quotes,
        pairs,
        consume_initial_stop,
        &[],
        &mut Ends::default(),
    )
}

#[cfg(test)]
#[path = "../../tests/unit/region_boundaries.rs"]
mod tests;
