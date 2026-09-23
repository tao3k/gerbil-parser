//! Generic contextual line structure over AOT-resolved syntax kinds.

use super::event_tree::build_rowan_events;
use super::lexer::line_end;
use super::model::{
    BlockLineRule, Diagnostic, HeadingLineRule, KindCategory, LanguageSpec, LineStructureSpec,
    Parse, ParseError, SelectiveGlrReceipt, TreeEvent, UnclosedBlockPolicy,
};
use super::validation::receipt;

/// Parse one source into nested sections, opaque blocks, and text lines.
///
/// The language pack owns every delimiter and kind identity. This engine only
/// executes the generic line/context transitions and emits validated Rowan
/// events. An unclosed block follows its Scheme-declared recovery policy.
///
/// # Errors
///
/// Returns a parse error with the same language and source receipt as the
/// generated LR path when the structural table or Rowan event stream is invalid.
pub fn parse_structural_lines(
    language: &'static LanguageSpec,
    structure: &LineStructureSpec,
    source: &str,
) -> Result<Parse, ParseError> {
    let parse_receipt = receipt(language, source, Some(structure.parser_digest), None);
    let with_receipt = |diagnostic| ParseError {
        receipt: Box::new(parse_receipt.clone()),
        diagnostic: Box::new(diagnostic),
        selective_glr: None,
    };
    validate_structure(language, structure).map_err(with_receipt)?;
    let mut events = Vec::with_capacity(source.len() / 16 + 2);
    let mut sections = Vec::new();
    let mut block: Option<&BlockLineRule> = None;
    let mut missing_closer_until = vec![None; structure.blocks.len()];
    events.push(TreeEvent::StartNode(language.root_kind));
    let mut start = 0;
    while start < source.len() {
        let end = line_end(source, start).ok_or_else(|| {
            with_receipt(Diagnostic {
                reason_kind: "line-boundary",
                byte_offset: start,
                message: "source line does not start at a UTF-8 boundary".into(),
            })
        })?;
        let line = &source[start..end];
        if let Some(rule) = block {
            if directive(line, rule.closing, rule.case_insensitive, rule.indent, true) {
                token(&mut events, rule.end_token, start, end);
                events.push(TreeEvent::FinishNode);
                block = None;
            } else {
                emit_block_body(&mut events, rule, line, start, end).map_err(with_receipt)?;
            }
        } else if let Some((index, rule)) = structure.blocks.iter().enumerate().find(|(_, rule)| {
            directive(
                line,
                rule.opening,
                rule.case_insensitive,
                rule.indent,
                false,
            )
        }) {
            let missing_closer = rule.unclosed == UnclosedBlockPolicy::RecoverAsText
                && (missing_closer_until[index].is_some_and(|boundary| end <= boundary)
                    || match has_closing_line(source, end, rule, structure.heading) {
                        Ok(()) => false,
                        Err(boundary) => {
                            // Reuse the scan only until the heading boundary.
                            missing_closer_until[index] = Some(boundary);
                            true
                        }
                    });
            if missing_closer {
                text_line(&mut events, structure, start, end);
            } else {
                events.push(TreeEvent::StartNode(rule.block_node));
                emit_block_opening(&mut events, rule, line, start, end);
                block = Some(rule);
            }
        } else if let Some(level) = heading_level(line, structure.heading) {
            while sections.last().is_some_and(|parent| *parent >= level) {
                events.push(TreeEvent::FinishNode);
                sections.pop();
            }
            events.push(TreeEvent::StartNode(structure.heading.section_node));
            sections.push(level);
            events.push(TreeEvent::StartNode(structure.heading.heading_node));
            token(&mut events, structure.heading.heading_token, start, end);
            events.push(TreeEvent::FinishNode);
        } else {
            text_line(&mut events, structure, start, end);
        }
        start = end;
    }
    if block.is_some() {
        events.push(TreeEvent::FinishNode);
    }
    for _ in sections {
        events.push(TreeEvent::FinishNode);
    }
    events.push(TreeEvent::FinishNode);
    let green = build_rowan_events(language, source, &events).map_err(with_receipt)?;
    Ok(Parse {
        green,
        kinds: language.kinds,
        receipt: parse_receipt,
        selective_glr: SelectiveGlrReceipt {
            branch_budget: 0,
            branches_explored: 0,
            speculative_branches_explored: 0,
            max_speculative_depth: 0,
            merged_branches: 0,
            successful_completions: 1,
            distinct_completions: 1,
            winner_reason: "deterministic-structure",
            dynamic_score: 0,
        },
    })
}

fn emit_block_opening(
    events: &mut Vec<TreeEvent>,
    rule: &BlockLineRule,
    line: &str,
    start: usize,
    end: usize,
) {
    let Some(header) = rule.header else {
        token(events, rule.begin_token, start, end);
        return;
    };
    let indent = if rule.indent {
        line.len() - line.trim_start_matches([' ', '\t']).len()
    } else {
        0
    };
    let prefix_end = indent + rule.opening.len();
    token(events, rule.begin_token, start, start + prefix_end);
    let bytes = line.as_bytes();
    let argument_start = prefix_end
        + bytes[prefix_end..]
            .iter()
            .take_while(|byte| matches!(byte, b' ' | b'\t'))
            .count();
    token_nonempty(
        events,
        header.trivia_token,
        start + prefix_end,
        start + argument_start,
    );
    let argument_end = argument_start
        + bytes[argument_start..]
            .iter()
            .take_while(|byte| !matches!(byte, b' ' | b'\t' | b'\r' | b'\n'))
            .count();
    token_nonempty(
        events,
        header.argument_token,
        start + argument_start,
        start + argument_end,
    );
    token_nonempty(events, header.trivia_token, start + argument_end, end);
}

fn emit_block_body(
    events: &mut Vec<TreeEvent>,
    rule: &BlockLineRule,
    line: &str,
    start: usize,
    end: usize,
) -> Result<(), Diagnostic> {
    let Some(body_line) = rule.body_line else {
        token(events, rule.body_token, start, end);
        return Ok(());
    };
    let parts = key_value_parts(line, body_line.marker).ok_or_else(|| Diagnostic {
        reason_kind: "invalid-structural-body",
        byte_offset: start,
        message: "validated key-value block contains an invalid body line".into(),
    })?;
    events.push(TreeEvent::StartNode(body_line.node));
    token_nonempty(
        events,
        body_line.trivia_token,
        start,
        start + parts.key_start,
    );
    token(
        events,
        body_line.key_token,
        start + parts.key_start,
        start + parts.key_end,
    );
    token_nonempty(
        events,
        body_line.trivia_token,
        start + parts.key_end,
        start + parts.value_start,
    );
    token_nonempty(
        events,
        body_line.value_token,
        start + parts.value_start,
        start + parts.value_end,
    );
    token_nonempty(events, body_line.trivia_token, start + parts.value_end, end);
    events.push(TreeEvent::FinishNode);
    Ok(())
}

fn token(events: &mut Vec<TreeEvent>, kind: u16, start: usize, end: usize) {
    events.push(TreeEvent::Token { kind, start, end });
}

fn token_nonempty(events: &mut Vec<TreeEvent>, kind: u16, start: usize, end: usize) {
    if start < end {
        token(events, kind, start, end);
    }
}

fn text_line(events: &mut Vec<TreeEvent>, structure: &LineStructureSpec, start: usize, end: usize) {
    events.push(TreeEvent::StartNode(structure.text_node));
    token(events, structure.text_token, start, end);
    events.push(TreeEvent::FinishNode);
}

fn has_closing_line(
    source: &str,
    mut start: usize,
    rule: &BlockLineRule,
    heading: HeadingLineRule,
) -> Result<(), usize> {
    while start < source.len() {
        let Some(end) = line_end(source, start) else {
            return Err(source.len());
        };
        let line = &source[start..end];
        if rule.heading_bound && heading_level(line, heading).is_some() {
            return Err(start);
        }
        if directive(line, rule.closing, rule.case_insensitive, rule.indent, true) {
            return Ok(());
        }
        if rule
            .body_line
            .is_some_and(|body_line| key_value_parts(line, body_line.marker).is_none())
        {
            return Err(start);
        }
        start = end;
    }
    Err(source.len())
}

struct KeyValueParts {
    key_start: usize,
    key_end: usize,
    value_start: usize,
    value_end: usize,
}

fn key_value_parts(line: &str, marker: u8) -> Option<KeyValueParts> {
    let bytes = line.as_bytes();
    let key_start = bytes
        .iter()
        .take_while(|byte| matches!(byte, b' ' | b'\t'))
        .count()
        + 1;
    if bytes.get(key_start - 1) != Some(&marker) {
        return None;
    }
    let key_end = key_start + bytes[key_start..].iter().position(|byte| *byte == marker)?;
    let key = &bytes[key_start..key_end];
    if key.is_empty() || key.iter().any(u8::is_ascii_whitespace) {
        return None;
    }
    let mut content_end = bytes.len();
    while content_end > key_end && matches!(bytes[content_end - 1], b'\r' | b'\n') {
        content_end -= 1;
    }
    let mut value_start = key_end + 1;
    if bytes
        .get(value_start)
        .is_some_and(|byte| !matches!(byte, b' ' | b'\t' | b'\r' | b'\n'))
    {
        return None;
    }
    while value_start < content_end && matches!(bytes[value_start], b' ' | b'\t') {
        value_start += 1;
    }
    let mut value_end = content_end;
    while value_end > value_start && matches!(bytes[value_end - 1], b' ' | b'\t') {
        value_end -= 1;
    }
    Some(KeyValueParts {
        key_start,
        key_end,
        value_start,
        value_end,
    })
}

fn heading_level(line: &str, rule: HeadingLineRule) -> Option<usize> {
    let bytes = line.as_bytes();
    let level = bytes
        .iter()
        .take_while(|byte| **byte == rule.marker)
        .count();
    (level > 0 && bytes.get(level) == Some(&rule.separator)).then_some(level)
}

fn directive(line: &str, value: &str, case_insensitive: bool, indent: bool, closing: bool) -> bool {
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

fn validate_structure(language: &LanguageSpec, spec: &LineStructureSpec) -> Result<(), Diagnostic> {
    if !super::validation::canonical_sha256_digest(spec.parser_digest) {
        return Err(invalid_structure(
            "structural parser digest is not canonical SHA-256",
        ));
    }
    if spec.grammar_digest != language.grammar_digest {
        return Err(invalid_structure(
            "structural and grammar artifacts have different digests",
        ));
    }
    let mut references = vec![
        (spec.heading.section_node, KindCategory::Node),
        (spec.heading.heading_node, KindCategory::Node),
        (spec.heading.heading_token, KindCategory::Token),
        (spec.text_node, KindCategory::Node),
        (spec.text_token, KindCategory::Token),
    ];
    if spec.heading.marker.is_ascii_whitespace()
        || spec.heading.separator.is_ascii_whitespace() && spec.heading.separator != b' '
        || spec.heading.marker == spec.heading.separator
    {
        return Err(invalid_structure("invalid heading marker or separator"));
    }
    for rule in spec.blocks {
        if rule.opening.is_empty()
            || rule.closing.is_empty()
            || !rule.opening.is_ascii()
            || !rule.closing.is_ascii()
        {
            return Err(invalid_structure("block delimiters must be nonempty ASCII"));
        }
        references.extend([
            (rule.block_node, KindCategory::Node),
            (rule.begin_token, KindCategory::Token),
            (rule.body_token, KindCategory::Token),
            (rule.end_token, KindCategory::Token),
        ]);
        if let Some(body_line) = rule.body_line {
            if rule.unclosed != UnclosedBlockPolicy::RecoverAsText
                || !body_line.marker.is_ascii()
                || body_line.marker.is_ascii_whitespace()
            {
                return Err(invalid_structure(
                    "key-value blocks require text recovery and a non-space marker",
                ));
            }
            references.extend([
                (body_line.node, KindCategory::Node),
                (body_line.key_token, KindCategory::Token),
                (body_line.value_token, KindCategory::Token),
                (body_line.trivia_token, KindCategory::Token),
            ]);
        }
        if let Some(header) = rule.header {
            references.extend([
                (header.argument_token, KindCategory::Token),
                (header.trivia_token, KindCategory::Token),
            ]);
        }
    }
    if references.into_iter().any(|(kind, category)| {
        language
            .kinds
            .get(usize::from(kind))
            .is_none_or(|entry| entry.category != category)
    }) {
        return Err(invalid_structure(
            "structural rule references unknown or wrong-category kind",
        ));
    }
    Ok(())
}

fn invalid_structure(message: &'static str) -> Diagnostic {
    Diagnostic {
        reason_kind: "invalid-structural-aot",
        byte_offset: 0,
        message: message.into(),
    }
}
