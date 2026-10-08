//! Exercise the Rust ownership boundary through actual compiled Gerbil callbacks.
use gerbil_parser_native::NativeElement;
use gerbil_parser_native::{LanguageApi, NativeSession, RawResult};
use std::sync::atomic::{AtomicUsize, Ordering};

unsafe extern "C" {
    fn records_language_create() -> u64;
    fn gerbil_parser_language_release(handle: u64) -> i32;
    fn gerbil_parser_result_release(result: *mut RawResult);
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
    unsafe { gerbil_parser_result_release(result) };
    FREED.fetch_add(1, Ordering::Relaxed);
}
fn api() -> LanguageApi {
    let mut api = LanguageApi::linked(records_language_create);
    api.release = release;
    api.result_release = free;
    api
}

fn check_tokens(view: &gerbil_parser_native::NativeArtifactView<'_>, source: &str) {
    let mut offset = 0;
    for (ordinal, token) in view.tokens().enumerate() {
        assert_eq!(token.id(), u64::try_from(ordinal).unwrap());
        assert_eq!(token.range().start, offset);
        assert_eq!(token.text(), &source[offset..token.range().end]);
        assert_eq!(token.text().as_ptr(), source[offset..].as_ptr());
        assert!(
            view.catalog()
                .terminals()
                .iter()
                .any(|t| t.name == token.class())
        );
        assert_eq!(token.parent().is_some(), view.accepted());
        offset = token.range().end;
    }
    assert_eq!(offset, source.len());
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
            let view = parsed.view().expect("bound view");
            // The view uses the original C buffer: no Rust payload copy.
            let original = parsed.bytes().unwrap();
            assert!(original.starts_with(b"GPA1"));
            check_tokens(&view, source);
            let other = second.parse("b=1\n").expect("concurrent owned result");
            assert!(other.view().unwrap().accepted());
            assert_eq!(parsed.bytes().unwrap().as_ptr(), original.as_ptr());
            assert_eq!(view.accepted(), matches!(source, "" | "a=1\n" | "α=1\r\n"));
            if let Some(root) = view.root() {
                assert_eq!(root.kind(), "Document");
                assert_eq!(root.text(), source);
                assert_eq!(root.text().as_ptr(), source.as_ptr());
                for child in root.children() {
                    if let NativeElement::Node(assignment) = child {
                        assert_eq!(assignment.kind(), "Assignment");
                        assert_eq!(assignment.field("name").count(), 1);
                        assert_eq!(assignment.field("value").count(), 1);
                        assert_eq!(assignment.parent().unwrap().id(), root.id());
                    }
                }
            } else {
                assert!(!view.accepted());
            }
            println!("RUST-NATIVE-SOURCE-OK bytes={}", source.len());
        }
        let started = std::time::Instant::now();
        for i in 1..=100 {
            let parsed = language.parse("α=1\n").expect("repeat parse");
            assert!(parsed.view().unwrap().accepted());
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
    assert_eq!(FREED.load(Ordering::Relaxed), 113);
    println!("RUST-NATIVE-OWNERSHIP-OK handles=2 results=113");
}

/// Called by the compiled Scheme test probe while the VM is alive.
#[unsafe(no_mangle)]
pub extern "C" fn gerbil_parser_rust_native_probe() -> i32 {
    match std::panic::catch_unwind(check) {
        Ok(()) => 0,
        Err(_) => -1,
    }
}

/// Independent C entry: Rust owns setup, handle/result lifetimes and cleanup.
#[cfg(feature = "standalone")]
#[unsafe(no_mangle)]
pub extern "C" fn gerbil_parser_rust_native_host_probe() -> i32 {
    let result = std::panic::catch_unwind(|| {
        println!("RUST-HOST-SETUP");
        // SAFETY: a fresh independent process links exactly this standalone
        // bundle, which stays loaded until process exit. No other VM is live.
        let mut runtime = unsafe {
            gerbil_parser_native::NativeRuntime::start(gerbil_parser_native::RuntimeApi::linked(
                records_language_create,
            ))
        }
        .expect("runtime setup");
        println!("RUST-HOST-READY");
        {
            let language = runtime.language().expect("owned language");
            let parsed = language.parse("α=1\n").expect("owned parse");
            assert!(parsed.view().unwrap().accepted());
        }
        check();
        runtime.close().expect("normal SDK cleanup");
        runtime.close().expect("Rust close is idempotent");
        assert!(matches!(
            runtime.language(),
            Err(gerbil_parser_native::NativeError::RuntimeClosed)
        ));
        // SAFETY: the linked library is still loaded; admission guards reject
        // both a new session and VM restart without entering the cleaned VM.
        assert!(matches!(
            unsafe { NativeSession::attach(api()) },
            Err(gerbil_parser_native::NativeError::OwnerThread)
        ));
        assert!(matches!(
            unsafe {
                gerbil_parser_native::NativeRuntime::start(
                    gerbil_parser_native::RuntimeApi::linked(records_language_create),
                )
            },
            Err(gerbil_parser_native::NativeError::Runtime(-3))
        ));
        println!("RUST-HOST-CLEANUP-OK");
    });
    if result.is_ok() { 0 } else { -1 }
}
