//! Exercise the Rust ownership boundary through actual compiled Gerbil callbacks.
use gerbil_parser_native::{LanguageApi, NativeSession, RawResult};
use gerbil_parser_rowan::{SyntaxNode, parse_contextual};
use std::sync::atomic::{AtomicUsize, Ordering};
#[path = "../../../../../rust/gerbil-parser-rowan/tests/fixtures/records_contextual_generated.rs"]
mod generated;

unsafe extern "C" {
    fn records_language_create() -> u64;
    fn gerbil_parser_language_release(handle: u64) -> i32;
    fn gerbil_parser_result_v2_release(result: *mut RawResult);
}
static RELEASED: AtomicUsize = AtomicUsize::new(0);
static FREED: AtomicUsize = AtomicUsize::new(0);

unsafe extern "C" fn release(handle: u64) -> i32 {
    // SAFETY: the session supplies its own live handle on the owner thread.
    let status = unsafe { gerbil_parser_language_release(handle) };
    if status == 0 {
        RELEASED.fetch_add(1, Ordering::Relaxed);
    }
    status
}
unsafe extern "C" fn free(result: *mut RawResult) {
    // SAFETY: the session supplies its initialized, exclusive owned result.
    unsafe { gerbil_parser_result_v2_release(result) };
    FREED.fetch_add(1, Ordering::Relaxed);
}
fn api() -> LanguageApi {
    let mut api = LanguageApi::linked(records_language_create);
    api.release = release;
    api.result_release = free;
    api
}

fn check() {
    RELEASED.store(0, Ordering::Relaxed);
    FREED.store(0, Ordering::Relaxed);
    // Admission on a foreign OS thread must fail before invoking Scheme.
    std::thread::spawn(|| {
        // SAFETY: the linked API is live; its owner guard is safe on any thread.
        assert!(matches!(
            unsafe { NativeSession::attach(api()) },
            Err(gerbil_parser_native::NativeError::OwnerThread)
        ));
    })
    .join()
    .expect("foreign thread");
    // SAFETY: called synchronously from the initialized Gerbil module's owner;
    // Scheme does not shut down or unload modules until this function returns.
    let session = unsafe { NativeSession::attach(api()) }.expect("owner admission");
    {
        let language = session.language().expect("language");
        let second = session.language().expect("independent handle");
        {
            let descriptor = language.descriptor().expect("descriptor");
            assert!(
                std::str::from_utf8(descriptor.bytes().unwrap())
                    .unwrap()
                    .contains("grammarDigest")
            );
        }
        for source in ["", "a=1\n", "α=1\r\n", "a=1\n\0", "a=!"] {
            let parsed = language.parse(source).expect("transport");
            let view = parsed
                .contextual_view(&generated::CONTEXTUAL, 3)
                .expect("bound view");
            // The view uses the original C buffer: no Rust payload copy.
            let original = parsed.bytes().unwrap();
            assert!(original.starts_with(b"GPA1"));
            let other = second.parse("b=1\n").expect("concurrent owned result");
            assert!(
                other
                    .contextual_view(&generated::CONTEXTUAL, 3)
                    .unwrap()
                    .accepted()
            );
            assert_eq!(parsed.bytes().unwrap().as_ptr(), original.as_ptr());
            let expected = parse_contextual(&generated::CONTEXTUAL, source);
            assert_eq!(view.accepted(), expected.is_ok());
            if let Ok(expected) = expected {
                let actual = SyntaxNode::new_root(view.to_rowan().unwrap());
                assert_eq!(format!("{actual:#?}"), format!("{:#?}", expected.syntax()));
                assert_eq!(actual.to_string(), source);
            } else {
                assert_eq!(view.to_rowan().unwrap_err().reason, "rejected-syntax");
            }
            println!("RUST-NATIVE-SOURCE-OK bytes={}", source.len());
        }
        let started = std::time::Instant::now();
        for i in 1..=100 {
            let parsed = language.parse("α=1\n").expect("repeat parse");
            assert!(
                parsed
                    .contextual_view(&generated::CONTEXTUAL, 3)
                    .unwrap()
                    .accepted()
            );
            if i % 25 == 0 {
                println!("RUST-NATIVE-PARSE-OK calls={i}");
            }
        }
        println!(
            "RUST-NATIVE-100-CALLS wall-ms={:.3}",
            started.elapsed().as_secs_f64() * 1000.0
        );
    }
    assert_eq!(RELEASED.load(Ordering::Relaxed), 2);
    assert_eq!(FREED.load(Ordering::Relaxed), 111);
    println!("RUST-NATIVE-OWNERSHIP-OK handles=2 results=111");
}

/// Called by the compiled Scheme test probe while the VM is alive.
#[unsafe(no_mangle)]
pub extern "C" fn gerbil_parser_rust_native_probe() -> i32 {
    match std::panic::catch_unwind(check) {
        Ok(()) => 0,
        Err(_) => -1,
    }
}
