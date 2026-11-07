use sha2::{Digest, Sha256};
use std::{env, fs, path::PathBuf};

fn main() {
    let source = PathBuf::from("src/parser.table");
    println!("cargo:rerun-if-changed={}", source.display());
    let artifact = fs::read(&source).expect("external parser artifact");
    let digest = format!("sha256:{:x}", Sha256::digest(artifact));
    let output = PathBuf::from(env::var("OUT_DIR").expect("OUT_DIR"));
    fs::write(
        output.join("provider_digest.rs"),
        format!("pub const PROVIDER_DIGEST: &str = {digest:?};\n"),
    )
    .expect("write build-bound provider digest");
}
