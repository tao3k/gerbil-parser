//! Build-only verification of Scheme-owned typed Rust function IR.

#[test]
fn scheme_ir_compiles_to_owned_rest_function() {
    let ir = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../t/fixtures/owned_rest_after_first_word.ir.json"
    ));
    gerbil_scheme_rust_ir::compile_function_json(ir).unwrap();
}

#[test]
fn scheme_event_fold_compiles_to_stateful_rowan_events() {
    let ir = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../t/fixtures/event_fold.ir.json"
    ));
    let rust = gerbil_scheme_rust_ir::compile_event_function_json(ir).unwrap();
    assert!(rust.contains("pub fn parse_fold_lines"));
    assert!(rust.contains("paragraph_open"));
    assert!(rust.contains("TreeEvent::Token"));
    let digest_start = rust.find("sha256:").unwrap();
    let digest = &rust[digest_start..digest_start + 71];
    let generated = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../rust/gerbil-parser-rowan/tests/unit/event_fold_generated.rs"
    ));
    assert!(generated.contains(digest), "generated Rust is stale");
}

#[test]
fn scheme_outline_fold_compiles_one_marker_scan_per_line() {
    let ir = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../t/fixtures/outline_fold.ir.json"
    ));
    let rust = gerbil_scheme_rust_ir::compile_event_function_json(ir).unwrap();
    assert_eq!(rust.matches("take_while").count(), 1);
    let digest_start = rust.find("sha256:").unwrap();
    let digest = &rust[digest_start..digest_start + 71];
    let generated = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../rust/gerbil-parser-rowan/tests/unit/outline_fold_generated.rs"
    ));
    assert!(
        generated.contains(digest),
        "generated outline Rust is stale"
    );
    assert_eq!(generated.matches("take_while").count(), 1);
}
