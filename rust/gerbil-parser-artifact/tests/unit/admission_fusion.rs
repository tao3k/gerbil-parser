//! Preserve complete admission decisions and every navigation entry.
use super::{Navigation, reference_navigation};
use crate::NativeCatalog;
fn take<'a>(input: &mut &'a [u8]) -> &'a [u8] {
    let (length, rest) = input.split_at(4);
    let length = u32::from_le_bytes(length.try_into().unwrap()) as usize;
    let (value, rest) = rest.split_at(length);
    *input = rest;
    value
}
fn parity(payload: &[u8], source: &str, catalog: &NativeCatalog) {
    let candidate = Navigation::decode(payload, source, catalog);
    let control = reference_navigation::decode(payload, source, catalog);
    match (candidate, control) {
        (Ok(candidate), Ok(control)) => {
            assert_eq!(candidate.accepted, control.accepted);
            assert_eq!(candidate.index, control.index);
        }
        (Err(candidate), Err(control)) => assert_eq!(candidate, control),
        (candidate, control) => panic!("admission mismatch: {candidate:?} / {control:?}"),
    }
}
#[test]
fn fused_admission_matches_former_decoder_for_all_native_languages_and_corrupt_rows() {
    let mut input = include_bytes!("../fixtures/languages.bin").as_slice();
    let mut languages = 0;
    while !input.is_empty() {
        let _name = take(&mut input);
        let descriptor = take(&mut input);
        let source = std::str::from_utf8(take(&mut input)).unwrap();
        let payload = take(&mut input);
        input = &input[1..];
        let catalog = NativeCatalog::from_descriptor(descriptor).unwrap();
        parity(payload, source, &catalog);
        for at in 0..payload.len() {
            let mut damaged = payload.to_vec();
            damaged[at] ^= 0x80;
            parity(&damaged, source, &catalog);
        }
        // Extra/truncated records and source identity remain rejected equally.
        for length in [0, 4, 79, payload.len() - 1] {
            parity(&payload[..length], source, &catalog);
        }
        parity(payload, "wrong source", &catalog);
        languages += 1;
    }
    assert_eq!(languages, 20);
}
