//! Frozen second traversal used only by the complete-request benchmark control.
use super::{Index, Navigation};
use crate::{NativeArtifactError, NativeCatalog, NativeEventKind as Kind, wire};
#[path = "reference_admission.rs"]
mod reference_admission;
pub(super) fn decode(
    payload: &[u8],
    source: &str,
    catalog: &NativeCatalog,
) -> Result<Navigation, NativeArtifactError> {
    let accepted = reference_admission::admit(payload, source, catalog)?;
    let count = (payload.len() - 80) / 24;
    let mut index = vec![
        Index {
            end: 0,
            parent: None
        };
        count
    ];
    let mut stack = Vec::new();
    let mut nodes = Vec::new();
    for (at, row) in payload[80..].as_chunks::<24>().0.iter().enumerate() {
        let event = wire::event(row);
        index[at].parent = nodes.last().copied();
        match event.kind {
            Kind::StartNode | Kind::StartField => {
                stack.push(at);
                if event.kind == Kind::StartNode {
                    nodes.push(at);
                }
            }
            Kind::FinishNode | Kind::FinishField => {
                let start = stack
                    .pop()
                    .ok_or_else(|| wire::error("index-nesting", Some(at)))?;
                index[start].end = at;
                if event.kind == Kind::FinishNode {
                    nodes.pop();
                }
            }
            Kind::Token => {
                index[at].end = at;
            }
        }
    }
    Ok(Navigation { accepted, index })
}
