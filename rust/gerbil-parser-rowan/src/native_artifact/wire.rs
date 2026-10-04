//! Fixed-width little-endian GPA1 records; admission precedes public iteration.
use std::fmt;

/// Canonical node, field and token record tags from the Scheme producer.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum NativeEventKind {
    StartNode,
    FinishNode,
    StartField,
    FinishField,
    Token,
}

/// Symbol indexes refer to the bound descriptor's kinds, fields, or terminals.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct NativeEvent {
    pub kind: NativeEventKind,
    pub symbol: u32,
    pub id: u64,
    pub start: usize,
    pub end: usize,
}

/// A rejected wire invariant, with its event index when applicable.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct NativeArtifactError {
    pub reason: &'static str,
    pub event: Option<usize>,
}
impl fmt::Display for NativeArtifactError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "native artifact {} at event {:?}",
            self.reason, self.event
        )
    }
}
impl std::error::Error for NativeArtifactError {}

pub(super) fn error(reason: &'static str, event: Option<usize>) -> NativeArtifactError {
    NativeArtifactError { reason, event }
}
pub(super) fn u32_at(bytes: &[u8], offset: usize) -> u32 {
    u32::from_le_bytes(
        bytes[offset..offset + 4]
            .try_into()
            .expect("admitted record width"),
    )
}
pub(super) fn event(row: &[u8]) -> NativeEvent {
    NativeEvent {
        kind: match u32_at(row, 0) {
            1 => NativeEventKind::StartNode,
            2 => NativeEventKind::FinishNode,
            3 => NativeEventKind::StartField,
            4 => NativeEventKind::FinishField,
            5 => NativeEventKind::Token,
            _ => unreachable!("admitted event tag"),
        },
        symbol: u32_at(row, 4),
        id: u64::from_le_bytes(row[8..16].try_into().expect("admitted record width")),
        start: u32_at(row, 16) as usize,
        end: u32_at(row, 20) as usize,
    }
}

// Admission proves every node symbol fits Rowan's catalog index width.
pub(super) fn kind_index(symbol: u32) -> u16 {
    u16::try_from(symbol).expect("admitted kind")
}
