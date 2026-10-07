//! Real configured Scheme products admitted without generated Rust parser tables.
use crate::{NativeArtifactView, NativeCatalog};
// Fixture records are four little-endian length-prefixed byte slices and a status.
const CASES: &[u8] = include_bytes!("../fixtures/languages.bin");

fn take_bytes<'a>(input: &mut &'a [u8]) -> &'a [u8] {
    let (length, rest) = input.split_at(4);
    let length = u32::from_le_bytes(length.try_into().unwrap()) as usize;
    let (value, rest) = rest.split_at(length);
    *input = rest;
    value
}

#[test]
fn all_production_entries_publish_accepted_and_rejected_native_results() {
    let mut input = CASES;
    let mut controls = 0;
    let mut entries = std::collections::HashMap::new();
    while !input.is_empty() {
        let name = std::str::from_utf8(take_bytes(&mut input)).unwrap();
        let descriptor = take_bytes(&mut input);
        let source = take_bytes(&mut input);
        let payload = take_bytes(&mut input);
        let accepted = match input[0] {
            0 => false,
            1 => true,
            status => panic!("invalid fixture status {status}"),
        };
        input = &input[1..];
        controls += 1;
        let statuses = entries.entry(name).or_insert([false; 2]);
        assert!(!statuses[usize::from(accepted)], "duplicate control {name}");
        statuses[usize::from(accepted)] = true;
        let catalog = NativeCatalog::from_descriptor(descriptor).expect(name);
        let source = std::str::from_utf8(source).expect(name);
        let view = NativeArtifactView::decode(payload, source, &catalog).expect(name);
        assert_eq!(view.accepted(), accepted, "{name}");
        if let Some(root) = view.root() {
            assert_eq!(root.kind(), catalog.root_kind(), "{name}");
            assert_eq!(root.text(), source, "{name}");
        } else {
            assert!(!accepted, "{name}");
        }
        let mut offset = 0;
        for (ordinal, token) in view.tokens().enumerate() {
            assert_eq!(token.id(), u64::try_from(ordinal).unwrap(), "{name}");
            assert_eq!(token.range().start, offset, "{name}");
            assert_eq!(token.text(), &source[offset..token.range().end], "{name}");
            assert_eq!(token.text().as_ptr(), source[offset..].as_ptr(), "{name}");
            assert_eq!(token.parent().is_some(), accepted, "{name}");
            offset = token.range().end;
        }
        assert_eq!(offset, source.len(), "{name}");
    }
    assert_eq!(controls, 20);
    assert_eq!(entries.len(), 10);
    assert!(entries.values().all(|statuses| *statuses == [true; 2]));
}
