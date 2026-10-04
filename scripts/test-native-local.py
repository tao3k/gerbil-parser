#!/usr/bin/env python3
"""Qualify host-native Rust and compiled Gerbil GQL with separate build/test fences."""

import argparse
import os
from pathlib import Path
import platform
import shutil
import subprocess
import sys
import tempfile


def native_environment():
    environment = os.environ.copy()
    cargo = shutil.which("cargo")
    if not cargo:
        rustup = Path.home() / ".cargo/bin/rustup"
        cargo = subprocess.check_output([str(rustup), "which", "cargo"], text=True, timeout=5).strip()
    tool_directory = Path(cargo).resolve().parent
    # Select cargo and rustc together. Avoid Nix cc wrappers in an Apple build.
    gerbil = shutil.which("gxi")
    native_tools = [str(tool_directory)]
    if gerbil:
        native_tools.append(str(Path(gerbil).resolve().parent))
    environment["PATH"] = os.pathsep.join(
        native_tools + ["/opt/homebrew/bin", "/usr/local/bin",
         "/usr/bin", "/bin", "/usr/sbin", "/sbin"])
    excluded = []
    for name in list(environment):
        if (name.startswith("CARGO_PROFILE_") or name in {
            "NIX_CC", "NIX_CFLAGS_COMPILE", "NIX_LDFLAGS", "CARGO_BUILD_TARGET",
            "RUSTFLAGS", "CARGO_ENCODED_RUSTFLAGS", "RUSTC", "RUSTDOC", "RUSTC_WRAPPER",
            "RUSTC_WORKSPACE_WRAPPER",
        }):
            excluded.append(name)
            del environment[name]
    if platform.system() == "Darwin":
        environment.update(CC="/usr/bin/clang", CXX="/usr/bin/clang++")
        environment["SDKROOT"] = subprocess.check_output(
            ["/usr/bin/xcrun", "--show-sdk-path"], text=True, timeout=5).strip()
        version = subprocess.check_output([str(tool_directory / "rustc"), "-vV"],
                                          env=environment, text=True, timeout=5)
        host = next(line.removeprefix("host: ") for line in version.splitlines()
                    if line.startswith("host: "))
        environment["CARGO_TARGET_" + host.upper().replace("-", "_") + "_LINKER"] = "/usr/bin/clang"
        print("NATIVE-TOOLCHAIN", host, "SDK", environment["SDKROOT"], flush=True)
    if excluded:
        print("NATIVE-ENV excluded overrides:", ",".join(sorted(excluded)), flush=True)
    return environment


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--suite", choices=["rust", "gql", "gql-profile", "gql-actors", "ffi", "aot"], action="append")
    parser.add_argument("--gerbil-path", type=Path, default=Path(".gerbil"))
    parser.add_argument("--log-dir", type=Path, default=Path(".data/local-native"))
    parser.add_argument("--jobs", type=int, default=min(4, os.cpu_count() or 1))
    args = parser.parse_args()
    if args.jobs < 1:
        parser.error("jobs must be positive")
    root = Path(__file__).resolve().parent.parent
    os.chdir(root)
    environment = native_environment()
    environment["RUSTC_WRAPPER"] = str(root / "scripts/rustc-progress.py")
    native_root = args.gerbil_path.resolve()
    # Immutable CI releases are relocated. Preserve their ~~ runtime mappings;
    # replacing GAMBOPT makes gxc look for gsc at the release builder's path.
    runtime_options = environment.get("GAMBOPT", "")
    heap_options = "max-heap=1G,debug=q"
    environment.update(GERBIL_PATH=str(native_root), GERBIL_LOADPATH=os.pathsep.join([str(native_root / "lib"), str(root)]),
                       GAMBOPT=runtime_options + ("," if runtime_options else "") + heap_options,
                       GERBIL_PARSER_LR_TRACE="1", GERBIL_BUILD_VERBOSE="1")
    logs = args.log_dir.resolve()
    logs.mkdir(parents=True, exist_ok=True)

    def run(label, command, *, build=False, required=(), timeout=None):
        print("NATIVE-STAGE", label, flush=True)
        bounded = [sys.executable, str(root / "scripts/run-bounded.py"),
                   "--timeout", str(timeout or (240 if build else 120)),
                   "--idle-timeout", "30" if build else "5",
                   "--log", str(logs / (label + ".log"))]
        for marker in required:
            bounded += ["--require", marker]
        result = subprocess.run(bounded + ["--"] + command, env=environment)
        if result.returncode:
            raise SystemExit(result.returncode)
        print("NATIVE-STAGE-OK", label, flush=True)

    suites = args.suite or ["rust", "gql", "ffi", "aot"]
    if any(suite in ("gql", "gql-profile", "gql-actors", "ffi") for suite in suites):
        run("test-driver-build", ["gxi", "build-native-test-driver.ss", "compile"], build=True)
    for suite in suites:
        if suite == "rust":
            # A cold compiler and a running test have different evidence boundaries.
            run("rust-build", ["cargo", "test", "--workspace", "--locked", "--no-run",
                               "--jobs", str(args.jobs), "--verbose"], build=True)
            run("rust-test", ["cargo", "test", "--workspace", "--locked", "--",
                              "--nocapture"], required=[r"test result: ok\."])
        elif suite == "gql-profile":
            run("gql-profile-build", ["gxc", "-V",
                "languages/gql/iso-39075-2024/benchmarks/runtime/matched-stages.ss"], build=True)
            run("gql-profile", ["gxi", "-e",
                '(load "t/fixtures/tla-sany-differential/preload.ss") (prefer-native-interfaces!) (preload-module "gerbil-parser/languages/gql/iso-39075-2024/benchmarks/runtime/matched-stages") (preload-module "gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process")', "-e",
                '(import :gerbil-parser/languages/gql/iso-39075-2024/benchmarks/runtime/matched-stages :gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process) (main "40" "100") (test-child-process-exit! 0)'],
                required=["GQL-STAGES-OK", "GQL-STAGE-SUMMARY"])
        elif suite == "gql-actors":
            run("gql-actors-build", ["gxc", "-V",
                "languages/gql/iso-39075-2024/benchmarks/runtime/matched-stages.ss",
                "languages/gql/iso-39075-2024/benchmarks/runtime/actors.ss"], build=True)
            run("gql-actors", ["gxi", "-e",
                '(load "t/fixtures/tla-sany-differential/preload.ss") (prefer-native-interfaces!) (preload-module "gerbil-parser/languages/gql/iso-39075-2024/benchmarks/runtime/actors") (preload-module "gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process")', "-e",
                '(import :gerbil-parser/languages/gql/iso-39075-2024/benchmarks/runtime/actors :gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process) (main "40" "100") (test-child-process-exit! 0)'],
                required=["GQL-ACTORS-OK", "GQL-STAGE-SUMMARY"])
        elif suite == "aot":
            run("aot-build", ["gxi", "build-rust-rowan-aot.ss", "compile"], build=True)
            gerbil_home = subprocess.check_output(
                ["gxi", "-e", '(display (gerbil-home))'], env=environment, text=True, timeout=5).strip()
            fixture = "t/fixtures/rowan-record-assignments"
            with tempfile.TemporaryDirectory(prefix="parser-native-aot-") as directory:
                generated = str(Path(directory) / "records.rs")
                run("aot-closed", ["env", "-i", "HOME=" + str(Path.home()),
                    "PATH=/usr/bin:/bin", "GERBIL_PATH=" + str(native_root),
                    "GERBIL_PARSER_ROWAN_AOT_LIB=" + str(native_root / "lib") + ":" + gerbil_home + "/lib",
                    str(native_root / "bin/gerbil-parser-rowan-aot"),
                    fixture + "/languages/records/v1/grammar.ss", generated])
                run("aot-format", ["rustfmt", "--edition", "2024", generated])
                run("aot-compare", ["cmp", generated, fixture + "/src/generated/records_v1.rs"])
        else:
            if suite == "ffi":
                run("ffi-header", [environment.get("CC", "cc"), "-std=c11", "-Wall",
                    "-Wextra", "-Werror", "-Wno-unused-command-line-argument",
                    "-fsyntax-only", "-Iinclude", "t/native-ffi-header-smoke.c"])
                run("ffi-abi-build", ["gxi", "build-native-ffi-tests.ss", "compile"], build=True)
                run("ffi-abi", ["gxi", "-e",
                    '(load "t/fixtures/tla-sany-differential/preload.ss") (prefer-native-interfaces!) (preload-module "gerbil-parser/t/fixtures/native-ffi/abi-probe") (preload-module "gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process")', "-e",
                    '(import :gerbil-parser/t/fixtures/native-ffi/abi-probe :gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process) (main) (test-child-process-exit! 0)'],
                    required=["NATIVE-ABI-OK", "ABI-ROWAN-GENERATED-MATCH calls=3"])
                run("native-language-abi", ["gxi", "-e",
                    '(load "t/fixtures/tla-sany-differential/preload.ss") (prefer-native-interfaces!) (preload-module "gerbil-parser/t/fixtures/native-ffi/language-v2-probe")', "-e",
                    '(import :gerbil-parser/t/fixtures/native-ffi/language-v2-probe) (main)'],
                    required=["LANGUAGE-ABI-OK", "LANGUAGE-ABI-100-CALLS"])
                environment["CARGO_TARGET_DIR"] = str(root / "target")
                run("rust-native-build", ["cargo", "build", "--locked", "--manifest-path",
                    "t/fixtures/rust-native-language/Cargo.toml"], build=True, timeout=90)
                environment["GERBIL_PARSER_RUST_NATIVE_ARCHIVE"] = str(
                    root / "target/debug/librust_native_language_fixture.a")
                run("rust-native-link", ["gxi", "-e",
                    '(load "t/fixtures/tla-sany-differential/preload.ss") (prefer-native-interfaces!) (preload-module "std/build-script")',
                    "build-rust-native-tests.ss", "compile"], build=True, timeout=90)
                run("rust-native-ownership", ["gxi", "-e",
                    '(load "t/fixtures/tla-sany-differential/preload.ss") (prefer-native-interfaces!) (preload-module "gerbil-parser/t/fixtures/native-ffi/rust-language-probe")', "-e",
                    '(import :gerbil-parser/t/fixtures/native-ffi/rust-language-probe) (main)'],
                    required=["RUST-NATIVE-OK", "RUST-NATIVE-OWNERSHIP-OK handles=2 results=111",
                              "RUST-NATIVE-100-CALLS"], timeout=90)
            modules = (["languages/gql/iso-39075-2024/parser", "src/runtime/parser"]
                       if suite == "gql" else ["src/ffi/rust-rowan-aot-v1"])
            for module in modules:
                # Reject interpreter-only metadata. Gerbil loads the compiled .o1.
                object_file = native_root / "lib/gerbil-parser" / (module + ".o1")
                if not object_file.is_file():
                    raise SystemExit(f"missing compiled module {object_file}; build this checkout first")
            if suite == "gql":
                run("gql-profile-build", ["gxc", "-V",
                    "languages/gql/iso-39075-2024/benchmarks/runtime/matched-stages.ss"], build=True)
            files = (["languages/gql/iso-39075-2024/runtime-benchmark-test.ss",
                      "languages/gql/iso-39075-2024/benchmark-profile-test.ss",
                      "languages/gql/iso-39075-2024/actor-test.ss"] if suite == "gql"
                     else ["t/native-ffi-test.ss", "t/build-product-contract-test.ss",
                           "t/native-language-test.ss", "t/native-language-benchmark-test.ss"])
            run(suite, ["gxi", "t/fixtures/tla-sany-differential/native-suite.ss"] + files,
                required=[f"NATIVE-SUITE-OK modules={len(files)}", r"^OK$"])
    print("LOCAL-NATIVE-OK " + ",".join(suites), flush=True)
    return 0


if __name__ == "__main__":
    sys.exit(main())
