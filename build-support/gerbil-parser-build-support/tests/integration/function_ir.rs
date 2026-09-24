//! Build-only verification of Scheme-owned typed Rust function IR.

#[test]
fn scheme_ir_compiles_to_owned_rest_function() {
    let ir = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../t/fixtures/owned_rest_after_first_word.ir.json"
    ));
    gerbil_scheme_rust_ir::compile_function_json(ir).unwrap();
}
