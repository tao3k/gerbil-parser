//! Thin application binding to the upstream program lifecycle.
use gerbil_scheme::{GerbilRuntime, LinkedGerbilProgram};

unsafe extern "C" {
    #[link_name = "___LNK_parser__rust__fixture"]
    fn fixture_linker(
        state: *mut gerbil_scheme_sys::GerbilGlobalState,
    ) -> *mut gerbil_scheme_sys::GerbilModuleOrLink;
    fn gerbil_parser_verify_rust_ownership() -> i32;
}

pub fn verify_owned_program() {
    // SAFETY: the upstream builder verifies the compiler-owned graph includes
    // exactly one ABI-v1 bridge. Its entry returns normally and lives forever.
    let program = unsafe { LinkedGerbilProgram::from_linker(fixture_linker) };
    let runtime = GerbilRuntime::initialize_program(program).expect("initialize ABI fixture owner");
    // SAFETY: setup initialized this graph; this synchronous owner-thread export
    // catches Scheme exceptions and retains no Rust pointers or handles.
    let status = unsafe { gerbil_parser_verify_rust_ownership() };
    drop(runtime);
    assert_eq!(status, 0, "Scheme/Rust ownership contract");
    println!("RUST-ABI-OWNER-CLEANUP-OK");
}
