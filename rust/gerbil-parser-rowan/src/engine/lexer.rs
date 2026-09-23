//! Generated-rule lexical execution and validation of downstream scanned tokens.

use std::collections::HashMap;

use super::model::{Diagnostic, LanguageSpec, LexicalExpr, LexicalRule, ScannedToken, Token};

pub(crate) fn lex_scanned<'source>(
    spec: &LanguageSpec,
    source: &'source str,
    scanned: &[ScannedToken],
) -> Result<(Vec<Token<'source>>, Vec<usize>), Diagnostic> {
    let terminals: HashMap<_, _> = spec
        .terminals
        .iter()
        .filter_map(|terminal| {
            spec.lexical_rules
                .iter()
                .find(|rule| rule.terminal == terminal.name)
                .map(|rule| (terminal.name, (terminal.syntax_kind, rule.extra)))
        })
        .collect();
    let mut tokens = Vec::with_capacity(scanned.len());
    let mut significant = Vec::with_capacity(scanned.len());
    let mut offset = 0;
    for item in scanned {
        if item.start != offset
            || item.end <= item.start
            || item.end > source.len()
            || !source.is_char_boundary(item.start)
            || !source.is_char_boundary(item.end)
        {
            return Err(Diagnostic {
                reason_kind: "scanner-range",
                byte_offset: item.start.min(source.len()),
                message: "scanner tokens must cover the source in ordered nonempty UTF-8 ranges"
                    .into(),
            });
        }
        let Some(&(syntax_kind, extra)) = terminals.get(item.terminal) else {
            return Err(Diagnostic {
                reason_kind: "scanner-terminal",
                byte_offset: item.start,
                message: format!("scanner emitted undeclared terminal {}", item.terminal),
            });
        };
        if !extra {
            significant.push(tokens.len());
        }
        tokens.push(Token {
            terminal: item.terminal,
            syntax_kind,
            text: &source[item.start..item.end],
            start: item.start,
            end: item.end,
        });
        offset = item.end;
    }
    if offset != source.len() {
        return Err(Diagnostic {
            reason_kind: "scanner-range",
            byte_offset: offset,
            message: "scanner tokens do not cover the source suffix".into(),
        });
    }
    Ok((tokens, significant))
}

pub(crate) fn lex<'source>(
    spec: &LanguageSpec,
    source: &'source str,
) -> Result<(Vec<Token<'source>>, Vec<usize>), Diagnostic> {
    let mut offset = 0;
    let token_capacity = source.len().min(256);
    let mut tokens = Vec::with_capacity(token_capacity);
    let mut significant = Vec::with_capacity(token_capacity);
    while offset < source.len() {
        if !source.is_char_boundary(offset) {
            return Err(Diagnostic {
                reason_kind: "lexer-offset",
                byte_offset: offset,
                message: "lexer offset is not a UTF-8 boundary".into(),
            });
        }
        let mut selected: Option<(&LexicalRule, usize, usize)> = None;
        for (order, rule) in spec.lexical_rules.iter().enumerate() {
            let Some(end) = lexical_end(&rule.expression, source, offset) else {
                continue;
            };
            if end <= offset {
                continue;
            }
            let replace = selected.is_none_or(|(current, current_end, current_order)| {
                end > current_end
                    || (end == current_end && rule.precedence > current.precedence)
                    || (end == current_end
                        && rule.precedence == current.precedence
                        && order < current_order)
            });
            if replace {
                selected = Some((rule, end, order));
            }
        }
        let Some((rule, end, _)) = selected else {
            return Err(Diagnostic {
                reason_kind: "lexical-rejected",
                byte_offset: offset,
                message: "no generated lexical rule consumed the source".into(),
            });
        };
        let Some(syntax_kind) = spec
            .terminals
            .iter()
            .find(|terminal| terminal.name == rule.terminal)
            .map(|terminal| terminal.syntax_kind)
        else {
            return Err(Diagnostic {
                reason_kind: "invalid-aot-artifact",
                byte_offset: offset,
                message: format!("unknown generated terminal {}", rule.terminal),
            });
        };
        if !rule.extra {
            significant.push(tokens.len());
        }
        tokens.push(Token {
            terminal: rule.terminal,
            syntax_kind,
            text: &source[offset..end],
            start: offset,
            end,
        });
        offset = end;
    }
    Ok((tokens, significant))
}

pub(crate) fn lexical_end(expression: &LexicalExpr, source: &str, offset: usize) -> Option<usize> {
    let suffix = &source[offset..];
    match expression {
        LexicalExpr::Whitespace => consume_while(source, offset, char::is_whitespace),
        LexicalExpr::HorizontalWhitespace => {
            consume_while(source, offset, |character| matches!(character, ' ' | '\t'))
        }
        LexicalExpr::Newline => {
            consume_while(source, offset, |character| matches!(character, '\r' | '\n'))
        }
        LexicalExpr::Line => line_end(source, offset),
        LexicalExpr::DecimalDigits => consume_while(source, offset, char::is_numeric),
        LexicalExpr::Number => number_end(source, offset),
        LexicalExpr::NumberLiteral {
            prefixes,
            separator,
            suffixes,
            leading_period,
            trailing_period,
        } => number_literal_end(
            source,
            offset,
            prefixes,
            separator,
            suffixes,
            *leading_period,
            *trailing_period,
        ),
        LexicalExpr::Identifier => identifier_end(source, offset),
        LexicalExpr::UntilDelimiters(delimiters) => {
            consume_while(source, offset, |character| !delimiters.contains(character))
        }
        LexicalExpr::QuotedString(delimiters) => delimiters
            .iter()
            .find_map(|delimiter| quoted_string_end(source, offset, delimiter, true)),
        LexicalExpr::EscapedQuotedString(delimiters) => delimiters
            .iter()
            .find_map(|delimiter| quoted_string_end(source, offset, delimiter, false)),
        LexicalExpr::Heredoc => heredoc_end(source, offset),
        LexicalExpr::LineComment(prefixes) => line_comment_end(source, offset, prefixes),
        LexicalExpr::BlockComment { opening, closing } => {
            block_comment_end(source, offset, opening, closing, false)
        }
        LexicalExpr::NestedBlockComment { opening, closing } => {
            block_comment_end(source, offset, opening, closing, true)
        }
        LexicalExpr::Choice(expressions) => expressions
            .iter()
            .filter_map(|expression| lexical_end(expression, source, offset))
            .max(),
        LexicalExpr::Literals(values) => values
            .iter()
            .filter(|value| suffix.starts_with(**value))
            .max_by_key(|value| value.len())
            .map(|value| offset + value.len()),
        LexicalExpr::Fallback => suffix
            .chars()
            .next()
            .map(|character| offset + character.len_utf8()),
    }
}

pub(crate) fn line_end(source: &str, offset: usize) -> Option<usize> {
    let tail = source.get(offset..)?;
    if tail.is_empty() {
        return None;
    }
    for (relative, byte) in tail.bytes().enumerate() {
        match byte {
            b'\n' => return Some(offset + relative + 1),
            b'\r' => {
                let end = offset + relative + 1;
                return Some(end + usize::from(source.as_bytes().get(end) == Some(&b'\n')));
            }
            _ => {}
        }
    }
    Some(source.len())
}

fn longest_literal<'a>(source: &str, offset: usize, values: &'a [&str]) -> Option<&'a str> {
    values
        .iter()
        .copied()
        .filter(|value| source[offset..].starts_with(value))
        .max_by_key(|value| value.len())
}

fn quoted_string_end(
    source: &str,
    offset: usize,
    delimiter: &str,
    doubled_delimiter: bool,
) -> Option<usize> {
    if delimiter.is_empty() || !source[offset..].starts_with(delimiter) {
        return None;
    }
    let mut cursor = offset + delimiter.len();
    while cursor < source.len() {
        if source[cursor..].starts_with('\\') {
            cursor += '\\'.len_utf8();
            let escaped = source[cursor..].chars().next()?;
            cursor += escaped.len_utf8();
        } else if source[cursor..].starts_with(delimiter) {
            let next = cursor + delimiter.len();
            if doubled_delimiter && source[next..].starts_with(delimiter) {
                cursor = next + delimiter.len();
            } else {
                return Some(next);
            }
        } else {
            cursor += source[cursor..].chars().next()?.len_utf8();
        }
    }
    None
}

fn line_comment_end(source: &str, offset: usize, prefixes: &[&str]) -> Option<usize> {
    let prefix = longest_literal(source, offset, prefixes)?;
    let body = offset + prefix.len();
    Some(
        source[body..]
            .char_indices()
            .find_map(|(relative, character)| {
                matches!(character, '\n' | '\r').then_some(body + relative)
            })
            .unwrap_or(source.len()),
    )
}

fn block_comment_end(
    source: &str,
    offset: usize,
    opening: &str,
    closing: &str,
    nested: bool,
) -> Option<usize> {
    if opening.is_empty() || closing.is_empty() || !source[offset..].starts_with(opening) {
        return None;
    }
    let mut cursor = offset + opening.len();
    let mut depth = 1_usize;
    while cursor < source.len() {
        if nested && source[cursor..].starts_with(opening) {
            depth += 1;
            cursor += opening.len();
        } else if source[cursor..].starts_with(closing) {
            depth -= 1;
            cursor += closing.len();
            if depth == 0 {
                return Some(cursor);
            }
        } else {
            cursor += source[cursor..].chars().next()?.len_utf8();
        }
    }
    None
}

fn identifier_end(source: &str, offset: usize) -> Option<usize> {
    let mut characters = source[offset..].char_indices();
    let (_, first) = characters.next()?;
    if first != '_' && !first.is_alphabetic() {
        return None;
    }
    let mut end = offset + first.len_utf8();
    for (_, character) in characters {
        if character != '_'
            && character != '-'
            && !character.is_alphabetic()
            && !character.is_numeric()
        {
            break;
        }
        end += character.len_utf8();
    }
    Some(end)
}

fn number_end(source: &str, offset: usize) -> Option<usize> {
    let whole_end = consume_while(source, offset, char::is_numeric)?;
    let fraction_end = if source[whole_end..].starts_with('.') {
        let digits_start = whole_end + 1;
        consume_while(source, digits_start, char::is_numeric).unwrap_or(whole_end)
    } else {
        whole_end
    };
    let Some(exponent) = source[fraction_end..].chars().next() else {
        return Some(fraction_end);
    };
    if !matches!(exponent, 'e' | 'E') {
        return Some(fraction_end);
    }
    let mut digits_start = fraction_end + exponent.len_utf8();
    if let Some(sign) = source[digits_start..].chars().next()
        && matches!(sign, '+' | '-')
    {
        digits_start += sign.len_utf8();
    }
    consume_while(source, digits_start, char::is_numeric).or(Some(fraction_end))
}

fn number_literal_end(
    source: &str,
    offset: usize,
    prefixes: &[&str],
    separator: &str,
    suffixes: &[&str],
    leading_period: bool,
    trailing_period: bool,
) -> Option<usize> {
    let separator = separator.chars().next()?;
    let radix_end = longest_literal(source, offset, prefixes).and_then(|prefix| {
        let base = match prefix.chars().last()?.to_ascii_lowercase() {
            'b' => 2,
            'o' => 8,
            'x' => 16,
            _ => return None,
        };
        separated_digits_end(source, offset + prefix.len(), base, separator)
    });
    let number_end = radix_end.or_else(|| {
        decimal_mantissa_end(source, offset, separator, leading_period, trailing_period)
            .map(|mantissa| exponent_end(source, mantissa, separator))
    })?;
    Some(
        longest_literal(source, number_end, suffixes)
            .map_or(number_end, |suffix| number_end + suffix.len()),
    )
}

fn separated_digits_end(source: &str, offset: usize, base: u32, separator: char) -> Option<usize> {
    let mut characters = source[offset..].char_indices().peekable();
    let (_, first) = characters.next()?;
    first.to_digit(base)?;
    let mut end = offset + first.len_utf8();
    while let Some((relative, character)) = characters.next() {
        if character.is_digit(base) {
            end = offset + relative + character.len_utf8();
        } else if character == separator
            && characters
                .peek()
                .is_some_and(|(_, next)| next.is_digit(base))
        {
            let (digit_relative, digit) = characters.next().expect("peeked digit exists");
            end = offset + digit_relative + digit.len_utf8();
        } else {
            break;
        }
    }
    Some(end)
}

fn decimal_mantissa_end(
    source: &str,
    offset: usize,
    separator: char,
    leading_period: bool,
    trailing_period: bool,
) -> Option<usize> {
    if leading_period && source[offset..].starts_with('.') {
        return separated_digits_end(source, offset + 1, 10, separator);
    }
    let whole_end = separated_digits_end(source, offset, 10, separator)?;
    if !source[whole_end..].starts_with('.') {
        return Some(whole_end);
    }
    separated_digits_end(source, whole_end + 1, 10, separator)
        .or_else(|| trailing_period.then_some(whole_end + 1))
        .or(Some(whole_end))
}

fn exponent_end(source: &str, mantissa_end: usize, separator: char) -> usize {
    let Some(indicator) = source[mantissa_end..].chars().next() else {
        return mantissa_end;
    };
    if !matches!(indicator, 'e' | 'E') {
        return mantissa_end;
    }
    let mut digits_start = mantissa_end + indicator.len_utf8();
    if let Some(sign) = source[digits_start..].chars().next()
        && matches!(sign, '+' | '-')
    {
        digits_start += sign.len_utf8();
    }
    separated_digits_end(source, digits_start, 10, separator).unwrap_or(mantissa_end)
}

fn heredoc_end(source: &str, offset: usize) -> Option<usize> {
    if !source[offset..].starts_with("<<") {
        return None;
    }
    let mut marker_start = offset + 2;
    if source[marker_start..].starts_with('-') {
        marker_start += 1;
    }
    let marker_end = identifier_end(source, marker_start)?;
    let newline = source[marker_end..].chars().next()?;
    if !matches!(newline, '\n' | '\r') {
        return None;
    }
    let marker = &source[marker_start..marker_end];
    let mut line_start = marker_end + newline.len_utf8();
    while line_start < source.len() {
        let line_end = source[line_start..]
            .char_indices()
            .find_map(|(relative, character)| {
                matches!(character, '\n' | '\r').then_some(line_start + relative)
            })
            .unwrap_or(source.len());
        let content_start = source[line_start..line_end]
            .char_indices()
            .find_map(|(relative, character)| {
                (!matches!(character, ' ' | '\t')).then_some(line_start + relative)
            })
            .unwrap_or(line_end);
        if &source[content_start..line_end] == marker {
            return Some(line_end);
        }
        if line_end == source.len() {
            return None;
        }
        line_start = line_end + source[line_end..].chars().next()?.len_utf8();
    }
    None
}

fn consume_while(source: &str, offset: usize, predicate: impl Fn(char) -> bool) -> Option<usize> {
    let mut end = offset;
    for character in source[offset..].chars() {
        if !predicate(character) {
            break;
        }
        end += character.len_utf8();
    }
    (end > offset).then_some(end)
}
