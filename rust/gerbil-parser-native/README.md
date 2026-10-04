# Rust native language ownership

This crate calls the language-independent C ABI from an already initialized
Gerbil embedding host. It owns language handles and C result buffers. The
host must keep the VM and loaded modules alive until all sessions are dropped.
VM initialization and shutdown are not provided by this crate.

A language author generates the Scheme/C entry with
`generate-native-language-pack` and links that pack through the normal Gerbil
builder. Rust declares only the generated factory; the generic ABI functions
are supplied by `LanguageApi::linked`:

```rust
use gerbil_parser_native::{LanguageApi, NativeSession};
unsafe extern "C" {
    fn my_language_create() -> u64;
}
// SAFETY: this host has initialized the pack on this OS thread and will keep
// the VM and linked libraries alive until this scope and its borrowers end.
let session = unsafe { NativeSession::attach(LanguageApi::linked(my_language_create)) }?;
let language = session.language()?;
let parsed = language.parse(source)?;
let view = parsed.contextual_view(&generated::CONTEXTUAL, field_count)?;
let tree = view.to_rowan()?;
```

Use `view` for canonical packs and `contextual_view` for contextual packs.
The catalog and field count must come from the same generated language product.
A syntax rejection is a successful transport with `view.accepted() == false`.
Handles, results and views are tied together by Rust borrows. Sessions cannot
cross threads. Dropping a result releases its C allocation; dropping a language
releases its registered handle. The wrapper does not copy source or GPA1 bytes,
though the C/Scheme boundary still copies input and publishes a C-owned payload.

Verification uses a real Rust static consumer in
`t/fixtures/rust-native-language`, linked into the compiled Gerbil probe.
Run the repository's `scripts/test-native-local.py --suite ffi` or its native
Ubuntu CI. Compile-fail doctests verify the thread and borrow restrictions.

## Native qualification build

`build-rust-native-tests.ss` uses the official `std/build-script` / `std/make`
owner to compile and link the disposable Rust/C transport probe. Its
`optimize: #f` setting avoids whole-library optimizer analysis for this test
entry; Gambit still produces native code. Language packs and parser runtime
objects retain their own optimization settings. `verbose: 9` reports real
compiler work, and the fixture preloader traces imports throughout the build.

Both local and CI entrypoints use the native Cargo compiler progress proxy.
The Rust consumer build/link gates remain 90 seconds with 30 seconds of output
inactivity; execution remains 90 seconds with 5 seconds of output inactivity.
Qualification requires actual native handle/result ownership and 100-call
markers, not only a successful static Rust build.
