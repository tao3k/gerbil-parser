//! Validate identities, canonical records, nesting, and exact UTF-8 coverage.
use super::wire::{self, NativeArtifactError, NativeEventKind as Kind};
use crate::{KindCategory, LanguageSpec};
use sha2::{Digest, Sha256};

pub(super) fn admit(
    payload: &[u8],
    source: &str,
    language: &LanguageSpec,
    fields: usize,
    expected_digest: &str,
) -> Result<bool, NativeArtifactError> {
    let fail = |reason| wire::error(reason, None);
    if source.len() > 67_108_864
        || payload.len() < 80
        || &payload[..4] != b"GPA1"
        || wire::u32_at(payload, 4) != 1
        || wire::u32_at(payload, 8) > 1
    {
        return Err(fail("header"));
    }
    let count = wire::u32_at(payload, 12) as usize;
    if count.checked_mul(24).and_then(|n| n.checked_add(80)) != Some(payload.len()) {
        return Err(fail("length"));
    }
    let digest = expected_digest
        .strip_prefix("sha256:")
        .ok_or_else(|| fail("grammar-digest"))?;
    if digest.len() != 64
        || !digest.is_ascii()
        || !(0..32).all(|i| {
            u8::from_str_radix(&digest[i * 2..i * 2 + 2], 16).ok() == Some(payload[16 + i])
        })
    {
        return Err(fail("grammar-digest"));
    }
    if payload[48..80] != Sha256::digest(source.as_bytes())[..] {
        return Err(fail("source-digest"));
    }
    let accepted = wire::u32_at(payload, 8) == 0;
    validate_events(payload, source, language, fields, accepted)?;
    Ok(accepted)
}

fn validate_events(
    payload: &[u8],
    source: &str,
    language: &LanguageSpec,
    fields: usize,
    accepted: bool,
) -> Result<(), NativeArtifactError> {
    let (mut offset, mut nodes, mut tokens, mut roots) = (0, 0, 0, 0);
    let mut stack = Vec::new();
    for (index, row) in payload[80..].chunks_exact(24).enumerate() {
        let fail = |reason| wire::error(reason, Some(index));
        if !(1..=5).contains(&wire::u32_at(row, 0)) {
            return Err(fail("event-tag"));
        }
        let event = wire::event(row);
        match event.kind {
            Kind::StartNode => {
                if !accepted
                    || event.id != nodes
                    || event.start != offset
                    || event.end != 0
                    || (stack.is_empty()
                        && (roots != 0 || event.symbol != u32::from(language.root_kind)))
                    || u16::try_from(event.symbol).is_err()
                    || language
                        .kinds
                        .get(event.symbol as usize)
                        .is_none_or(|kind| kind.category != KindCategory::Node)
                {
                    return Err(fail("start-node"));
                }
                if stack.is_empty() {
                    roots += 1;
                }
                nodes += 1;
                stack.push((Kind::StartNode, event.id, event.symbol));
            }
            Kind::FinishNode => {
                if event.start != 0
                    || event.end != offset
                    || stack.pop() != Some((Kind::StartNode, event.id, event.symbol))
                {
                    return Err(fail("finish-node"));
                }
            }
            Kind::StartField => {
                if stack.is_empty()
                    || event.id != 0
                    || event.start != offset
                    || event.end != 0
                    || event.symbol as usize >= fields
                {
                    return Err(fail("start-field"));
                }
                stack.push((Kind::StartField, 0, event.symbol));
            }
            Kind::FinishField => {
                if event.id != 0
                    || event.start != 0
                    || event.end != offset
                    || stack.pop() != Some((Kind::StartField, 0, event.symbol))
                {
                    return Err(fail("finish-field"));
                }
            }
            Kind::Token => {
                if event.id != tokens
                    || event.start != offset
                    || event.end <= offset
                    || source.get(event.start..event.end).is_none()
                    || (accepted && stack.is_empty())
                    || language
                        .terminals
                        .get(event.symbol as usize)
                        .is_none_or(|terminal| {
                            language
                                .kinds
                                .get(usize::from(terminal.syntax_kind))
                                .is_none_or(|kind| kind.category != KindCategory::Token)
                        })
                {
                    return Err(fail("token"));
                }
                tokens += 1;
                offset = event.end;
            }
        }
    }
    if !stack.is_empty() || offset != source.len() || roots != usize::from(accepted) {
        return Err(wire::error("coverage", None));
    }
    Ok(())
}
