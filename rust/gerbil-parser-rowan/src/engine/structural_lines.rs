//! Generic contextual line structure over AOT-resolved syntax kinds.

use super::event_tree::build_rowan_events;
use super::lexer::line_end;
use super::model::{
    BlockLineRule, Diagnostic, HeadingLineRule, KindCategory, LanguageSpec, LineStructureSpec,
    Parse, ParseError, SelectiveGlrReceipt, TreeEvent,
};
use super::validation::receipt;

/// Parse one source into nested sections, opaque blocks, and text lines.
///
/// The language pack owns every delimiter and kind identity. This engine only
/// executes the generic line/context transitions and emits validated Rowan
/// events. An unclosed block remains lossless and is closed at EOF.
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
                token(&mut events, rule.body_token, start, end);
            }
        } else if let Some(rule) = structure.blocks.iter().find(|rule| {
            directive(
                line,
                rule.opening,
                rule.case_insensitive,
                rule.indent,
                false,
            )
        }) {
            events.push(TreeEvent::StartNode(rule.block_node));
            token(&mut events, rule.begin_token, start, end);
            block = Some(rule);
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
            events.push(TreeEvent::StartNode(structure.text_node));
            token(&mut events, structure.text_token, start, end);
            events.push(TreeEvent::FinishNode);
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

fn token(events: &mut Vec<TreeEvent>, kind: u16, start: usize, end: usize) {
    events.push(TreeEvent::Token { kind, start, end });
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
