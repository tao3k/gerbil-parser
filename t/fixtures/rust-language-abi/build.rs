#[cfg(not(feature = "embedded-runtime"))]
fn main() {}

#[cfg(feature = "embedded-runtime")]
fn main() {
    embedded::build();
}

#[cfg(feature = "embedded-runtime")]
mod embedded {
    // Package-specific inputs only: Gerbil and the upstream builder own compilation.
    use gerbil_scheme::GerbilRuntime;
    use gerbil_scheme_aot_build::{
        NativeHeaderInput, ProgramArchiveContract, ProgramArchiveObservation,
        ProgramArchiveObserver, ProgramArchiveRequest, build_program_archive_with_contract,
        discover_gambit_gsc_from_env, source_workspace,
    };
    use std::{env, fs, path::Path};

    struct CargoObserver {
        output: std::path::PathBuf,
    }
    impl ProgramArchiveObserver for CargoObserver {
        fn observe(&self, event: ProgramArchiveObservation<'_>) {
            eprintln!("[parser-rust-abi] {event}");
        }
        fn observe_source_input(&self, input: &Path) {
            if !input.starts_with(&self.output) {
                println!("cargo:rerun-if-changed={}", input.display());
            }
        }
    }

    pub fn build() {
        let fixture = env::var_os("CARGO_MANIFEST_DIR").unwrap();
        let fixture = Path::new(&fixture);
        let package = fixture.ancestors().nth(3).expect("Parser package root");
        let output = env::var_os("OUT_DIR").unwrap();
        let output = Path::new(&output);
        let stage = output.join("gerbil-program");
        fs::create_dir_all(&stage).expect("program staging directory");
        for name in [
            "GERBIL_HOME",
            "GERBIL_PATH",
            "GERBIL_LOADPATH",
            "GERBIL_GSC",
            "GAMBOPT",
            "PKG_CONFIG_PATH",
        ] {
            println!("cargo:rerun-if-env-changed={name}");
        }
        let source = fixture.join("program.ss");
        println!("cargo:rerun-if-changed={}", source.display());
        println!(
            "cargo:rerun-if-changed={}",
            fixture.join("build.ss").display()
        );
        println!(
            "cargo:rerun-if-changed={}",
            package
                .join("t/fixtures/ffi/rust-language-probe.ss")
                .display()
        );
        let runtime = GerbilRuntime::initialize().expect("upstream C ABI build owner");
        let prefix = output.join("gerbil-package");
        fs::create_dir_all(prefix.join("lib")).expect("isolated package products");
        let mut libraries = vec![prefix.join("lib")];
        if let Some(installed) = env::var_os("GERBIL_PATH") {
            libraries.push(Path::new(&installed).join("lib"));
        }
        if let Some(search) = env::var_os("GERBIL_LOADPATH") {
            libraries.extend(env::split_paths(&search));
        }
        let libraries = env::join_paths(libraries).expect("package search paths");
        assert_eq!(
            runtime
                .configure_package_paths(prefix.to_str().unwrap(), libraries.to_str().unwrap())
                .expect("bind Cargo-owned package products"),
            0
        );

        assert_eq!(
            runtime
                .build_package(source_workspace().to_str().unwrap())
                .expect("canonical bridge package build"),
            0
        );
        assert_eq!(
            runtime
                .build_package(fixture.to_str().unwrap())
                .expect("official package build"),
            0
        );
        assert_eq!(
            runtime
                .stage_package_program(source.to_str().unwrap(), stage.to_str().unwrap(), &[])
                .expect("Gerbil compiler staging"),
            0
        );
        drop(runtime);
        let include = package.join("include");
        let headers = [include.join("gerbil-parser/language.h")];
        let gsc = discover_gambit_gsc_from_env().expect("paired Gambit compiler");
        let receipt = build_program_archive_with_contract(
            ProgramArchiveRequest {
                manifest: &stage.join("program.json"),
                gsc: &gsc,
                archive_name: "parser_rust_fixture",
                linker_name: "parser_rust_fixture",
                out_dir: &stage,
            },
            ProgramArchiveContract {
                required_modules: &[
                    "gerbil-scheme-rust/scheme/runtime",
                    "gerbil-parser/src/ffi/language-abi",
                    "gerbil-parser/t/fixtures/shared-scanner/records-abi",
                    "gerbil-parser/t/fixtures/rust-language-abi/program",
                ],
                forbidden_modules: &[
                    "gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process",
                ],
                linker_main_symbol: "parser_rust_fixture_main",
                additional_objects: &[],
                native_headers: &[NativeHeaderInput {
                    include_directory: &include,
                    header_files: &headers,
                }],
            },
            &CargoObserver {
                output: output.to_path_buf(),
            },
        )
        .expect("upstream program archive");
        for directive in receipt.cargo_directives {
            println!("{}", directive.line());
        }
    }
}
