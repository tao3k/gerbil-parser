//! Cache reuse retains admission, exact catalog binding and original source slices.
use super::NativeArtifactView;
use crate::{NativeArtifactBytes, NativeArtifactError, NativeArtifactStorage, NativeCatalog};

struct Payload<'a>(&'a [u8]);
impl NativeArtifactBytes for Payload<'_> {
    fn bytes(&self) -> Result<&[u8], NativeArtifactError> {
        Ok(self.0)
    }
}
fn take<'a>(input: &mut &'a [u8]) -> &'a [u8] {
    let (length, rest) = input.split_at(4);
    let length = u32::from_le_bytes(length.try_into().unwrap()) as usize;
    let (value, rest) = rest.split_at(length);
    *input = rest;
    value
}
#[test]
fn cached_views_share_navigation_for_all_twenty_real_native_controls() {
    let mut input = include_bytes!("../fixtures/languages.bin").as_slice();
    let mut count = 0;
    while !input.is_empty() {
        let _name = take(&mut input);
        let descriptor = take(&mut input);
        let source = std::str::from_utf8(take(&mut input)).unwrap();
        let payload = take(&mut input);
        let accepted = input[0] == 1;
        input = &input[1..];
        let catalog = NativeCatalog::from_descriptor(descriptor).unwrap();
        let storage = NativeArtifactStorage::new(Payload(payload), source);
        let first = storage.view(&catalog).unwrap();
        let second = storage.view(&catalog).unwrap();
        assert_eq!(first.index.as_ptr(), second.index.as_ptr());
        assert_eq!(first.accepted(), accepted);
        assert_eq!(
            second.events().collect::<Vec<_>>(),
            first.events().collect::<Vec<_>>()
        );
        for token in second.tokens() {
            assert_eq!(
                token.text().as_ptr(),
                source[token.range().start..].as_ptr()
            );
        }
        drop(first);
        drop(second);
        let moved = storage;
        assert_eq!(moved.view(&catalog).unwrap().accepted(), accepted);
        let different_catalog = NativeCatalog::from_descriptor(descriptor).unwrap();
        assert_eq!(
            moved.view(&different_catalog).unwrap_err().reason,
            "native-view-catalog"
        );
        count += 1;
    }
    assert_eq!(count, 20);
}
#[test]
fn cached_storage_preserves_source_and_wire_rejection() {
    let mut input = include_bytes!("../fixtures/languages.bin").as_slice();
    let _name = take(&mut input);
    let catalog = NativeCatalog::from_descriptor(take(&mut input)).unwrap();
    let source = std::str::from_utf8(take(&mut input)).unwrap();
    let payload = take(&mut input);
    let wrong_source = NativeArtifactStorage::new(Payload(payload), "different source");
    assert_eq!(
        wrong_source.view(&catalog).unwrap_err().reason,
        NativeArtifactView::decode(payload, "different source", &catalog)
            .unwrap_err()
            .reason
    );
    let mut corrupt = payload.to_vec();
    corrupt[0] = 0;
    let corrupt_storage = NativeArtifactStorage::new(Payload(&corrupt), source);
    assert!(corrupt_storage.view(&catalog).is_err());
    assert!(corrupt_storage.view(&catalog).is_err());
}

#[test]
fn owner_cannot_rebind_cached_navigation_to_another_allocation() {
    struct Rebinding<'a>(std::cell::Cell<&'a [u8]>);
    impl NativeArtifactBytes for Rebinding<'_> {
        fn bytes(&self) -> Result<&[u8], NativeArtifactError> {
            Ok(self.0.get())
        }
    }
    let mut input = include_bytes!("../fixtures/languages.bin").as_slice();
    let _name = take(&mut input);
    let catalog = NativeCatalog::from_descriptor(take(&mut input)).unwrap();
    let source = std::str::from_utf8(take(&mut input)).unwrap();
    let payload = take(&mut input);
    let replacement = payload.to_vec();
    let storage = NativeArtifactStorage::new(Rebinding(std::cell::Cell::new(payload)), source);
    assert!(storage.view(&catalog).is_ok());
    storage.owner().0.set(&replacement);
    assert_eq!(
        storage.view(&catalog).unwrap_err().reason,
        "native-view-payload"
    );
}
