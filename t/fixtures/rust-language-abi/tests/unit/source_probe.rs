//! Own two transferred Source handles and exercise actual Scheme/C edit calls.
use gerbil_parser_ffi::{LanguageApi, NativeSession};
use std::cell::Cell;

unsafe extern "C" {
    fn records_language_create() -> u64;
    fn gerbil_parser_language_release(handle: u64) -> i32;
}
thread_local! {
    static TRANSFERRED: Cell<(u64, u64)> = const { Cell::new((0, 0)) };
}
unsafe extern "C" fn take_handle() -> u64 {
    TRANSFERRED.with(|handles| {
        let (first, second) = handles.get();
        handles.set((second, 0));
        first
    })
}
fn check_source() {
    let mut api = LanguageApi::linked(records_language_create);
    api.create = take_handle;
    // SAFETY: the Scheme probe transfers two live handles on the VM owner
    // thread. The VM and C callbacks remain live until this function returns.
    let session = unsafe { NativeSession::attach(api) }.expect("Source owner");
    let cached = session.language().expect("transferred Source handle");
    let fresh = session.language().expect("independent Source handle");
    let original = "cat <<A\nα\nA\necho end\n";
    let retained = cached.parse(original).expect("retained result");
    let retained_bytes = retained.bytes().unwrap().to_vec();
    assert!(retained.view().unwrap().accepted());
    for (source, accepted) in [
        ("echo猫 <<A\nα\nA\necho end\n", true),
        ("echo猫 <<'A'\nα\nA\necho end\n", true),
        ("cat <<A <<B\nα\nA\nβ\nB\necho end\n", true),
        ("cat <<A <<C\nα\nA\nβ\nC\necho end\n", true),
        ("if true; then\n", false),
        ("cat <<A\nα\n", false),
        ("echo done\n", true),
        ("", true),
        ("echo猫 <<A\nα\nA\necho end\n", true),
    ] {
        // An empty reference scan leaves no token history to reuse on input.
        drop(fresh.parse("").expect("clear reference token history"));
        let expected = fresh.parse(source).expect("fresh transport");
        let actual = cached.parse(source).expect("edit transport");
        assert_eq!(actual.bytes().unwrap(), expected.bytes().unwrap());
        let view = actual.view().expect("bound edited GPA1");
        assert_eq!(view.accepted(), accepted, "{source:?}");
        let mut offset = 0;
        for token in view.tokens() {
            assert_eq!(token.range().start, offset);
            assert_eq!(token.text(), &source[offset..token.range().end]);
            offset = token.range().end;
        }
        assert_eq!(offset, source.len());
        if let Some(root) = view.root() {
            assert_eq!(root.text(), source);
            assert_eq!(root.text().as_ptr(), source.as_ptr());
        }
        // Advancing a handle's history cannot mutate a still-live old payload.
        assert_eq!(retained.bytes().unwrap(), retained_bytes);
        assert_eq!(retained.view().unwrap().root().unwrap().text(), original);
        println!("RUST-NATIVE-SOURCE-EDIT-OK bytes={}", source.len());
    }
    println!("RUST-NATIVE-SOURCE-EDITS-OK");
}

/// Scheme transfers ownership of two admitted Source handles on its owner thread.
#[unsafe(no_mangle)]
pub extern "C" fn gerbil_parser_rust_native_source_probe(first: u64, second: u64) -> i32 {
    TRANSFERRED.with(|handles| handles.set((first, second)));
    let result = std::panic::catch_unwind(check_source);
    // A panic before acquisition must also release transferred handles.
    TRANSFERRED.with(|handles| {
        let (first, second) = handles.replace((0, 0));
        for handle in [first, second] {
            if handle != 0 {
                // SAFETY: these unacquired transferred handles are live and
                // exclusive, and this callback runs on the VM owner thread.
                unsafe { gerbil_parser_language_release(handle) };
            }
        }
    });
    if result.is_ok() { 0 } else { -1 }
}
