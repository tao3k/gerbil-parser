#!/usr/bin/env python3
"""Qualify normal SDK startup and cleanup from independent C and Rust hosts."""
import os
from pathlib import Path
import platform
import shlex
import subprocess
import sys
import tempfile


def main():
    root = Path(__file__).resolve().parent.parent
    environment = os.environ.copy()
    environment.update(CARGO_TARGET_DIR=str(root / "target"),
                       RUSTC_WRAPPER=str(root / "scripts/rustc-progress.py"))
    home = environment.get("GERBIL_HOME") or subprocess.check_output(
        ["gxi", "-e", "(display (gerbil-home))"], text=True, timeout=5).strip()
    compiler = shlex.split(subprocess.check_output(
        [str(Path(home) / "bin/gambuild-C"), "C_COMPILER"],
        text=True, timeout=5).strip())
    logs = root / ".data/native-host"
    logs.mkdir(parents=True, exist_ok=True)

    def run(name, command, *, build=False, marker=None):
        bounded = [sys.executable, str(root / "scripts/run-bounded.py"),
                   "--timeout", "90", "--idle-timeout", "30" if build else "5",
                   "--log", str(logs / (name + ".log"))]
        if marker:
            bounded += ["--require", marker]
        print("NATIVE-HOST-STAGE", name, flush=True)
        subprocess.run(bounded + ["--"] + command, cwd=root,
                       env=environment, check=True)

    with tempfile.TemporaryDirectory(prefix="gerbil-native-host-") as temporary:
        directory = Path(temporary)
        library = directory / ("libparsernative.dylib" if platform.system() == "Darwin"
                               else "libparsernative.so")
        run("bundle", [sys.executable, str(root / "scripts/build-native-library.py"),
                       "--output", str(library), "--language-module",
                       "gerbil-parser/t/fixtures/shared-scanner/records-native"],
            build=True, marker="NATIVE-BUNDLE-OK")
        run("rust-build", ["cargo", "build", "--locked", "--manifest-path",
                           "t/fixtures/rust-native-language/Cargo.toml",
                           "--features", "standalone"], build=True)
        host = directory / "native-host"
        run("link", compiler + [
            "-std=c11", "-D_POSIX_C_SOURCE=200809L", "-Iinclude", "-I.",
            "t/native-runtime-host.c", str(root / "target/debug/librust_native_language_fixture.a"),
            str(library), "-lpthread", "-lm"] +
            ([] if platform.system() == "Darwin" else ["-ldl"]) + ["-o", str(host)], build=True)
        run("c", [str(host)], marker="NATIVE-HOST-CLEANUP-OK")
        run("rust", [str(host), "rust"], marker="RUST-HOST-CLEANUP-OK")
    print("NATIVE-HOST-OK", flush=True)


if __name__ == "__main__":
    main()
