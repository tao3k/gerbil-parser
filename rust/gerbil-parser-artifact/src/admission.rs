//! Validate identities, canonical records, nesting, and exact UTF-8 coverage.
use super::wire::{self, NativeArtifactError, NativeEventKind as Kind};
use crate::view::{Index, Navigation};
use crate::{KindCategory, NativeCatalog};
use sha2::{Digest, Sha256};

pub(super) fn admit(
    payload: &[u8],
    source: &str,
    language: &NativeCatalog,
) -> Result<Navigation, NativeArtifactError> {
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
    let digest = language
        .grammar_digest
        .as_str()
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
    let index = validate_events(payload, source, language, language.fields.len(), accepted)?;
    Ok(Navigation { accepted, index })
}

fn validate_events(
    payload: &[u8],
    source: &str,
    language: &NativeCatalog,
    fields: usize,
    accepted: bool,
) -> Result<Vec<Index>, NativeArtifactError> {
    let (mut offset, mut nodes, mut tokens, mut roots) = (0, 0, 0, 0);
    let mut stack = Vec::new();
    // Do not reserve an untrusted declared event count before row admission.
    // Larger valid inputs grow with admitted records, not the wire header.
    let mut navigation = Vec::with_capacity(((payload.len() - 80) / 24).min(1024));
    let mut owner: Option<(usize, u32)> = None;
    for (index, row) in payload[80..].as_chunks::<24>().0.iter().enumerate() {
        let fail = |reason| wire::error(reason, Some(index));
        if !(1..=5).contains(&wire::u32_at(row, 0)) {
            return Err(fail("event-tag"));
        }
        let event = wire::event(row);
        navigation.push(Index {
            end: 0,
            parent: owner.map(|(at, _)| at),
        });
        match event.kind {
            Kind::StartNode => {
                if !accepted
                    || event.id != nodes
                    || event.start != offset
                    || event.end != 0
                    || (stack.is_empty()
                        && (roots != 0 || event.symbol as usize != language.root_kind))
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
                stack.push((Kind::StartNode, event.id, event.symbol, index, owner));
                owner = Some((index, event.symbol));
            }
            Kind::FinishNode => {
                let frame = stack.pop().ok_or_else(|| fail("finish-node"))?;
                if event.start != 0
                    || event.end != offset
                    || (frame.0, frame.1, frame.2) != (Kind::StartNode, event.id, event.symbol)
                {
                    return Err(fail("finish-node"));
                }
                navigation[frame.3].end = index;
                owner = frame.4;
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
                let owner_kind = owner.ok_or_else(|| fail("field-owner"))?.1;
                if !language.kinds[owner_kind as usize]
                    .fields
                    .contains(&language.fields[event.symbol as usize])
                {
                    return Err(fail("field-owner"));
                }
                stack.push((Kind::StartField, 0, event.symbol, index, owner));
            }
            Kind::FinishField => {
                let frame = stack.pop().ok_or_else(|| fail("finish-field"))?;
                if event.id != 0
                    || event.start != 0
                    || event.end != offset
                    || (frame.0, frame.1, frame.2) != (Kind::StartField, 0, event.symbol)
                {
                    return Err(fail("finish-field"));
                }
                navigation[frame.3].end = index;
            }
            Kind::Token => {
                if event.id != tokens
                    || event.start != offset
                    || event.end <= offset
                    || source.get(event.start..event.end).is_none()
                    || (accepted && stack.is_empty())
                    || event.symbol as usize >= language.terminals.len()
                {
                    return Err(fail("token"));
                }
                navigation[index].end = index;
                tokens += 1;
                offset = event.end;
            }
        }
    }
    if !stack.is_empty() || offset != source.len() || roots != usize::from(accepted) {
        return Err(wire::error("coverage", None));
    }
    Ok(navigation)
}
