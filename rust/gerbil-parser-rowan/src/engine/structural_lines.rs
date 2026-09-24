//! Generic contextual line structure over AOT-resolved syntax kinds.

use super::event_tree::build_rowan_events;
use super::lexer::line_end;
use super::model::{
    BlockContents, BlockLineRule, Diagnostic, HeadingLineRule, InlineLinkRule, KindCategory,
    LanguageSpec, LineStructureSpec, ListLineRule, Parse, ParseError, ParseReceipt,
    SelectiveGlrReceipt, TableLineRule, TreeEvent, UnclosedBlockPolicy,
};
use super::structural_key_line::{emit_key_line, key_line_references, matching_key_line};
use super::structural_list::{ListFrame, indent_column, list_marker};
use super::structural_table::{emit_table_row, is_table_line};
use super::validation::receipt;

/// Parse one source into nested sections, Scheme-declared blocks, and text lines.
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
    let mut state = StructuralState::new(language, structure, source);
    let mut start = 0;
    while start < source.len() {
        let end = structural_line_end(source, start).map_err(with_receipt)?;
        state
            .line(structure, source, start, end)
            .map_err(with_receipt)?;
        start = end;
    }
    close_structure(&mut state.events, &mut state.frames, state.sections.len());
    finish_structural_parse(language, source, &state.events, parse_receipt)
}

struct StructuralState {
    events: Vec<TreeEvent>,
    sections: Vec<usize>,
    frames: Vec<ElementFrame>,
    missing_closer_until: Vec<Option<usize>>,
    after_heading: bool,
}

impl StructuralState {
    fn new(language: &LanguageSpec, structure: &LineStructureSpec, source: &str) -> Self {
        let mut events = Vec::with_capacity(source.len() / 16 + 2);
        events.push(TreeEvent::StartNode(language.root_kind));
        Self {
            events,
            sections: Vec::new(),
            frames: vec![ElementFrame::root()],
            missing_closer_until: vec![None; structure.blocks.len()],
            after_heading: false,
        }
    }

    fn line(
        &mut self,
        structure: &LineStructureSpec,
        source: &str,
        start: usize,
        end: usize,
    ) -> Result<(), Diagnostic> {
        let line = &source[start..end];
        let active_block = self.frames.last().and_then(|frame| frame.block_index);
        let after_heading = std::mem::take(&mut self.after_heading);
        if let Some(rule) = active_block.map(|index| &structure.blocks[index]) {
            if directive(line, rule.closing, rule.case_insensitive, rule.indent, true) {
                let frame = self.frames.last_mut().expect("root frame is present");
                close_lists(&mut self.events, frame);
                token(&mut self.events, rule.end_token, start, end);
                self.events.push(TreeEvent::FinishNode);
                self.frames.pop();
                return Ok(());
            }
            if rule.contents == BlockContents::Opaque {
                emit_block_body(&mut self.events, rule, line, start, end)?;
                return Ok(());
            }
        }
        if let Some(rule) = structure.list {
            let is_heading =
                active_block.is_none() && heading_level(line, structure.heading).is_some();
            if !is_heading {
                let frame = self.frames.last_mut().expect("root frame is present");
                if consume_list_line(&mut self.events, frame, structure, rule, source, start..end) {
                    return Ok(());
                }
            }
        }
        if let Some((index, rule)) = structure.blocks.iter().enumerate().find(|(index, rule)| {
            Some(*index) != active_block
                && directive(
                    line,
                    rule.opening,
                    rule.case_insensitive,
                    rule.indent,
                    false,
                )
        }) {
            let frame = self.frames.last_mut().expect("root frame is present");
            close_table(&mut self.events, &mut frame.table_open);
            let missing_closer = missing_block_closer(
                source,
                end,
                rule,
                structure.heading,
                &mut self.missing_closer_until[index],
                active_block.map(|parent| &structure.blocks[parent]),
            );
            if missing_closer {
                paragraph_text_line(
                    &mut self.events,
                    structure,
                    source,
                    start,
                    end,
                    &mut frame.paragraph_open,
                );
            } else {
                close_paragraph(&mut self.events, &mut frame.paragraph_open);
                self.events.push(TreeEvent::StartNode(rule.block_node));
                emit_block_opening(&mut self.events, rule, line, start, end);
                self.frames.push(ElementFrame::block(index));
            }
        } else if active_block.is_none()
            && let Some(level) = heading_level(line, structure.heading)
        {
            let frame = self.frames.last_mut().expect("root frame is present");
            close_lists(&mut self.events, frame);
            emit_section_heading(
                &mut self.events,
                &mut self.sections,
                structure.heading,
                line,
                level,
                start,
                end,
            );
            self.after_heading = true;
        } else if let Some(rule) = matching_key_line(structure.key_lines, line, after_heading) {
            let frame = self.frames.last_mut().expect("root frame is present");
            close_lists(&mut self.events, frame);
            emit_key_line(&mut self.events, line, start, rule);
        } else if let Some(rule) = structure.table.filter(|rule| is_table_line(line, *rule)) {
            self.table_line(rule, line, start);
        } else {
            let frame = self.frames.last_mut().expect("root frame is present");
            close_table(&mut self.events, &mut frame.table_open);
            paragraph_text_line(
                &mut self.events,
                structure,
                source,
                start,
                end,
                &mut frame.paragraph_open,
            );
        }
        Ok(())
    }

    fn table_line(&mut self, rule: TableLineRule, line: &str, start: usize) {
        let frame = self.frames.last_mut().expect("root frame is present");
        emit_table_section_line(
            &mut self.events,
            rule,
            line,
            start,
            &mut frame.paragraph_open,
            &mut frame.table_open,
        );
    }
}

struct ElementFrame {
    block_index: Option<usize>,
    paragraph_open: bool,
    table_open: bool,
    lists: Vec<ListFrame>,
    blank_lines: usize,
}

impl ElementFrame {
    fn root() -> Self {
        Self::block_frame(None)
    }

    fn block(index: usize) -> Self {
        Self::block_frame(Some(index))
    }

    fn block_frame(block_index: Option<usize>) -> Self {
        Self {
            block_index,
            paragraph_open: false,
            table_open: false,
            lists: Vec::new(),
            blank_lines: 0,
        }
    }
}

fn close_one_list(events: &mut Vec<TreeEvent>, frame: &mut ElementFrame) {
    events.push(TreeEvent::FinishNode);
    events.push(TreeEvent::FinishNode);
    frame.lists.pop();
}

fn close_lists(events: &mut Vec<TreeEvent>, frame: &mut ElementFrame) {
    close_paragraph(events, &mut frame.paragraph_open);
    close_table(events, &mut frame.table_open);
    while !frame.lists.is_empty() {
        close_one_list(events, frame);
    }
    frame.blank_lines = 0;
}

fn consume_list_line(
    events: &mut Vec<TreeEvent>,
    frame: &mut ElementFrame,
    structure: &LineStructureSpec,
    rule: ListLineRule,
    source: &str,
    span: std::ops::Range<usize>,
) -> bool {
    let line = &source[span.clone()];
    if let Some(marker) = list_marker(line, rule) {
        close_paragraph(events, &mut frame.paragraph_open);
        close_table(events, &mut frame.table_open);
        while frame.lists.last().is_some_and(|list| {
            list.indent > marker.indent
                || list.indent == marker.indent && list.ordered != marker.ordered
        }) {
            close_one_list(events, frame);
        }
        if frame
            .lists
            .last()
            .is_some_and(|list| list.indent == marker.indent)
        {
            events.push(TreeEvent::FinishNode);
        } else {
            events.push(TreeEvent::StartNode(rule.list_node));
            frame.lists.push(ListFrame {
                indent: marker.indent,
                ordered: marker.ordered,
            });
        }
        events.push(TreeEvent::StartNode(rule.item_node));
        token_nonempty(
            events,
            rule.trivia_token,
            span.start,
            span.start + marker.bullet_start,
        );
        token(
            events,
            rule.bullet_token,
            span.start + marker.bullet_start,
            span.start + marker.bullet_end,
        );
        token_nonempty(
            events,
            rule.trivia_token,
            span.start + marker.bullet_end,
            span.start + marker.content_start,
        );
        if marker.content_start < line.trim_end_matches(['\r', '\n']).len() {
            paragraph_text_line(
                events,
                structure,
                source,
                span.start + marker.content_start,
                span.end,
                &mut frame.paragraph_open,
            );
        } else {
            token_nonempty(
                events,
                rule.trivia_token,
                span.start + marker.content_start,
                span.end,
            );
        }
        frame.blank_lines = 0;
        return true;
    }
    if frame.lists.is_empty() {
        return false;
    }
    if line.trim().is_empty() {
        frame.blank_lines += 1;
        if frame.blank_lines == 1 {
            close_paragraph(events, &mut frame.paragraph_open);
            close_table(events, &mut frame.table_open);
            token(events, rule.trivia_token, span.start, span.end);
            return true;
        }
        close_lists(events, frame);
        return false;
    }
    frame.blank_lines = 0;
    if frame
        .lists
        .last()
        .is_some_and(|list| indent_column(line, rule.tab_width) > list.indent)
    {
        return false;
    }
    close_lists(events, frame);
    false
}

fn missing_block_closer(
    source: &str,
    end: usize,
    rule: &BlockLineRule,
    heading: HeadingLineRule,
    cached_boundary: &mut Option<usize>,
    parent: Option<&BlockLineRule>,
) -> bool {
    if rule.unclosed != UnclosedBlockPolicy::RecoverAsText {
        return false;
    }
    if parent.is_none() && cached_boundary.is_some_and(|boundary| end <= boundary) {
        return true;
    }
    match has_closing_line(source, end, rule, heading, parent) {
        Ok(()) => false,
        Err(boundary) => {
            if parent.is_none() {
                *cached_boundary = Some(boundary);
            }
            true
        }
    }
}

fn structural_line_end(source: &str, start: usize) -> Result<usize, Diagnostic> {
    line_end(source, start).ok_or_else(|| Diagnostic {
        reason_kind: "line-boundary",
        byte_offset: start,
        message: "source line does not start at a UTF-8 boundary".into(),
    })
}

fn close_structure(
    events: &mut Vec<TreeEvent>,
    frames: &mut Vec<ElementFrame>,
    section_count: usize,
) {
    while let Some(mut frame) = frames.pop() {
        close_lists(events, &mut frame);
        if frame.block_index.is_some() {
            events.push(TreeEvent::FinishNode);
        }
    }
    for _ in 0..section_count {
        events.push(TreeEvent::FinishNode);
    }
    events.push(TreeEvent::FinishNode);
}

fn emit_section_heading(
    events: &mut Vec<TreeEvent>,
    sections: &mut Vec<usize>,
    heading: HeadingLineRule,
    line: &str,
    level: usize,
    start: usize,
    end: usize,
) {
    while sections.last().is_some_and(|parent| *parent >= level) {
        events.push(TreeEvent::FinishNode);
        sections.pop();
    }
    events.push(TreeEvent::StartNode(heading.section_node));
    sections.push(level);
    events.push(TreeEvent::StartNode(heading.heading_node));
    emit_heading(events, heading, line, level, start, end);
    events.push(TreeEvent::FinishNode);
}

fn emit_table_section_line(
    events: &mut Vec<TreeEvent>,
    rule: TableLineRule,
    line: &str,
    start: usize,
    paragraph_open: &mut bool,
    table_open: &mut bool,
) {
    close_paragraph(events, paragraph_open);
    if !*table_open {
        events.push(TreeEvent::StartNode(rule.table_node));
        *table_open = true;
    }
    emit_table_row(events, rule, line, start);
}

fn finish_structural_parse(
    language: &'static LanguageSpec,
    source: &str,
    events: &[TreeEvent],
    parse_receipt: ParseReceipt,
) -> Result<Parse, ParseError> {
    let with_receipt = |diagnostic| ParseError {
        receipt: Box::new(parse_receipt.clone()),
        diagnostic: Box::new(diagnostic),
        selective_glr: None,
    };
    let green = build_rowan_events(language, source, events).map_err(with_receipt)?;
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

fn emit_heading(
    events: &mut Vec<TreeEvent>,
    rule: HeadingLineRule,
    line: &str,
    level: usize,
    start: usize,
    end: usize,
) {
    let Some(fields) = rule.fields else {
        token(events, rule.heading_token, start, end);
        return;
    };
    token(events, rule.heading_token, start, start + level);
    let bytes = line.as_bytes();
    let title_start = level
        + bytes[level..]
            .iter()
            .take_while(|byte| matches!(byte, b' ' | b'\t'))
            .count();
    token_nonempty(
        events,
        fields.trivia_token,
        start + level,
        start + title_start,
    );
    let title_end = bytes[..]
        .iter()
        .rposition(|byte| !matches!(byte, b' ' | b'\t' | b'\r' | b'\n'))
        .map_or(title_start, |index| (index + 1).max(title_start));
    token_nonempty(
        events,
        fields.title_token,
        start + title_start,
        start + title_end,
    );
    token_nonempty(events, fields.trivia_token, start + title_end, end);
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

fn text_line(
    events: &mut Vec<TreeEvent>,
    structure: &LineStructureSpec,
    source: &str,
    start: usize,
    end: usize,
) {
    events.push(TreeEvent::StartNode(structure.text_node));
    if let Some(rule) = structure.inline_link {
        emit_inline_links(
            events,
            structure.text_token,
            rule,
            &source[start..end],
            start,
        );
    } else {
        token(events, structure.text_token, start, end);
    }
    events.push(TreeEvent::FinishNode);
}

fn paragraph_text_line(
    events: &mut Vec<TreeEvent>,
    structure: &LineStructureSpec,
    source: &str,
    start: usize,
    end: usize,
    paragraph_open: &mut bool,
) {
    if source[start..end]
        .bytes()
        .all(|byte| matches!(byte, b' ' | b'\t' | b'\r' | b'\n'))
    {
        close_paragraph(events, paragraph_open);
    } else if !*paragraph_open && let Some(kind) = structure.paragraph_node {
        events.push(TreeEvent::StartNode(kind));
        *paragraph_open = true;
    }
    text_line(events, structure, source, start, end);
}

fn close_paragraph(events: &mut Vec<TreeEvent>, paragraph_open: &mut bool) {
    if *paragraph_open {
        events.push(TreeEvent::FinishNode);
        *paragraph_open = false;
    }
}

fn close_table(events: &mut Vec<TreeEvent>, table_open: &mut bool) {
    if *table_open {
        events.push(TreeEvent::FinishNode);
        *table_open = false;
    }
}

fn emit_inline_links(
    events: &mut Vec<TreeEvent>,
    text_token: u16,
    rule: InlineLinkRule,
    line: &str,
    start: usize,
) {
    let mut cursor = 0;
    while let Some(relative_open) = line[cursor..].find(rule.opening) {
        let open = cursor + relative_open;
        let target_start = open + rule.opening.len();
        let Some(relative_close) = line[target_start..].find(rule.closing) else {
            break;
        };
        let close = target_start + relative_close;
        let separator = line[target_start..close]
            .find(rule.separator)
            .map(|offset| target_start + offset);
        let target_end = separator.unwrap_or(close);
        if target_end == target_start {
            break;
        }
        token_nonempty(events, text_token, start + cursor, start + open);
        events.push(TreeEvent::StartNode(rule.node));
        token(
            events,
            rule.trivia_token,
            start + open,
            start + target_start,
        );
        token(
            events,
            rule.target_token,
            start + target_start,
            start + target_end,
        );
        if let Some(separator) = separator {
            let description_start = separator + rule.separator.len();
            token(
                events,
                rule.trivia_token,
                start + separator,
                start + description_start,
            );
            token_nonempty(
                events,
                rule.description_token,
                start + description_start,
                start + close,
            );
        }
        let next = close + rule.closing.len();
        token(events, rule.trivia_token, start + close, start + next);
        events.push(TreeEvent::FinishNode);
        cursor = next;
    }
    token_nonempty(events, text_token, start + cursor, start + line.len());
}

fn has_closing_line(
    source: &str,
    mut start: usize,
    rule: &BlockLineRule,
    heading: HeadingLineRule,
    parent: Option<&BlockLineRule>,
) -> Result<(), usize> {
    while start < source.len() {
        let Some(end) = line_end(source, start) else {
            return Err(source.len());
        };
        let line = &source[start..end];
        if parent.is_some_and(|parent| {
            directive(
                line,
                parent.closing,
                parent.case_insensitive,
                parent.indent,
                true,
            )
        }) {
            return Err(start);
        }
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
    if let Some(kind) = spec.paragraph_node {
        references.push((kind, KindCategory::Node));
    }
    if let Some(rule) = spec.table {
        references.extend(table_references(rule)?);
    }
    if let Some(rule) = spec.list {
        references.extend(list_references(rule)?);
    }
    for rule in spec.key_lines {
        references.extend(key_line_references(*rule)?);
    }
    if let Some(fields) = spec.heading.fields {
        references.extend([
            (fields.title_token, KindCategory::Token),
            (fields.trivia_token, KindCategory::Token),
        ]);
    }
    if let Some(rule) = spec.inline_link {
        if [rule.opening, rule.separator, rule.closing]
            .iter()
            .any(|delimiter| delimiter.is_empty() || !delimiter.is_ascii())
        {
            return Err(invalid_structure(
                "inline link delimiters must be nonempty ASCII",
            ));
        }
        references.extend([
            (rule.node, KindCategory::Node),
            (rule.target_token, KindCategory::Token),
            (rule.description_token, KindCategory::Token),
            (rule.trivia_token, KindCategory::Token),
        ]);
    }
    if spec.heading.marker.is_ascii_whitespace()
        || spec.heading.separator.is_ascii_whitespace() && spec.heading.separator != b' '
        || spec.heading.marker == spec.heading.separator
    {
        return Err(invalid_structure("invalid heading marker or separator"));
    }
    for rule in spec.blocks {
        validate_block_rule(rule, &mut references)?;
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

fn validate_block_rule(
    rule: &BlockLineRule,
    references: &mut Vec<(u16, KindCategory)>,
) -> Result<(), Diagnostic> {
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
        if rule.contents == BlockContents::Elements {
            return Err(invalid_structure(
                "recursive element blocks cannot also require key-value body lines",
            ));
        }
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
    Ok(())
}

fn table_references(rule: TableLineRule) -> Result<[(u16, KindCategory); 8], Diagnostic> {
    if !rule.delimiter.is_ascii() || rule.delimiter.is_ascii_whitespace() {
        return Err(invalid_structure("table delimiter must be nonspace ASCII"));
    }
    Ok([
        (rule.table_node, KindCategory::Node),
        (rule.row_node, KindCategory::Node),
        (rule.rule_row_node, KindCategory::Node),
        (rule.cell_node, KindCategory::Node),
        (rule.separator_token, KindCategory::Token),
        (rule.cell_token, KindCategory::Token),
        (rule.trivia_token, KindCategory::Token),
        (rule.rule_token, KindCategory::Token),
    ])
}

fn list_references(rule: ListLineRule) -> Result<[(u16, KindCategory); 4], Diagnostic> {
    if rule.tab_width == 0
        || rule.tab_width > 16
        || rule.unordered_markers.is_empty()
        || !rule.unordered_markers.is_ascii()
        || rule
            .unordered_markers
            .as_bytes()
            .iter()
            .any(|byte| byte.is_ascii_whitespace() || byte.is_ascii_alphanumeric())
    {
        return Err(invalid_structure(
            "list markers must be nonempty, non-alphanumeric ASCII",
        ));
    }
    Ok([
        (rule.list_node, KindCategory::Node),
        (rule.item_node, KindCategory::Node),
        (rule.bullet_token, KindCategory::Token),
        (rule.trivia_token, KindCategory::Token),
    ])
}

fn invalid_structure(message: &'static str) -> Diagnostic {
    Diagnostic {
        reason_kind: "invalid-structural-aot",
        byte_offset: 0,
        message: message.into(),
    }
}
