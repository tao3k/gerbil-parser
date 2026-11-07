//! Build-time lowering only; the IR comes from the complete Scheme Org owner.
fn main() {
    let path = std::env::args().nth(1).expect("expected IR path");
    let wire = std::fs::read_to_string(path).expect("read Scheme IR");
    let source = gerbil_scheme_rust_ir::compile_event_function_json(&wire)
        .expect("complete Org Fold must lower, not a reduced fixture");
    println!("{source}");
}
