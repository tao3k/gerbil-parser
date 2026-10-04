#!/usr/bin/env python3
"""Build a standalone language bundle through the Gerbil executable compiler.

Only build orchestration lives here. Gerbil owns module closure, static AOT and
linking; the Gambit SDK owns runtime initialization/cleanup. Generated C is
neither decoded nor rewritten.
"""
import argparse
import os
from pathlib import Path
import platform
import re
import shlex
import shutil
import subprocess
import tempfile
import time
import uuid


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--language-module", action="append", required=True)
    args = parser.parse_args()
    for module in args.language_module:
        if not re.fullmatch(r"[A-Za-z0-9_./-]+", module):
            parser.error("language-module must be a Gerbil module identity")
    root = Path(__file__).resolve().parent.parent
    output = args.output.resolve()
    output.parent.mkdir(parents=True, exist_ok=True)
    home = os.environ.get("GERBIL_HOME") or subprocess.check_output(
        ["gxi", "-e", "(display (gerbil-home))"], text=True, timeout=5).strip()
    with tempfile.TemporaryDirectory(prefix="gerbil-native-bundle-") as temporary:
        directory = Path(temporary)
        source = directory / ("bundle" + uuid.uuid4().hex + ".ss")
        source.write_text("(import " + " ".join("(prefix-in :" + module + " language" + str(index) + "-)"
                                    for index, module in enumerate(args.language_module))
                          + ")\n(export main)\n(def (main . _) (void))\n")
        shim = directory / "runtime.o"
        print("NATIVE-BUNDLE-RUNTIME-COMPILE", flush=True)
        subprocess.run(shlex.split(os.environ.get("CC", "cc")) + [
            "-std=c11", "-fPIC", "-I" + str(Path(home) / "include"),
            "-I" + str(root / "include"),
            "-DGERBIL_PARSER_LINKER=___LNK_gpembed____exe__",
            "-c", str(root / "native/runtime.c"), "-o", str(shim)], check=True)
        # A fixed compiler output basename defines the SDK linker identifier.
        # The source module identity is unique to avoid static-cache collisions.
        bundle = directory / "gpembed"
        if platform.system() == "Darwin":
            linker = "-dynamiclib -Wl,-install_name," + str(output)
        else:
            linker = "-shared -Wl,-z,defs"
        environment = os.environ.copy()
        if environment.get("CC"):
            # The SDK exposes GERBIL_GSC, but compile-exe filters -cc out of
            # its gsc-options. Carry the official option at this entrypoint.
            # This transient command adapter never replaces the SDK runtime.
            compiler = directory / "gsc-native"
            compiler.write_text("#!/bin/sh\nexec " + shlex.join([
                str(Path(home) / "bin/gsc"), "-:~~=" + home,
                "-cc", environment["CC"]]) + ' "$@"\n')
            compiler.chmod(0o700)
            environment["GERBIL_GSC"] = str(compiler)
        preload = '(gx#import-module (quote :gerbil/compiler) #f #t)'
        for module in args.language_module:
            preload += ' (gx#import-module (string->symbol ' + '"' + ":" + module + '"' + ') #f #t)'
        preload += ' (void)'
        print("NATIVE-BUNDLE-GERBIL-AOT", flush=True)
        command = ["gxi", "-e", preload, str(root / "build-native-library.ss"), str(source),
            str(bundle), "-save-temps=obj -fPIC -fwrapv -fno-strict-aliasing -foptimize-sibling-calls "
            + "-I" + str(root / "include"), linker + " " + str(shim)]
        # Report actual preprocessing/IR/assembly/object writes. These are real
        # compiler artifacts, not periodic heartbeats or simulated progress.
        observed = {shim: (shim.stat().st_size, shim.stat().st_mtime_ns)}
        process = subprocess.Popen(command, env=environment)
        while process.poll() is None:
            for artifact in directory.rglob("*"):
                if artifact.suffix not in {".i", ".bc", ".s", ".o"}:
                    continue
                try:
                    stat = artifact.stat()
                except FileNotFoundError:
                    continue
                state = (stat.st_size, stat.st_mtime_ns)
                if stat.st_size and observed.get(artifact) != state:
                    observed[artifact] = state
                    print("NATIVE-COMPILER-OUTPUT", artifact.relative_to(directory),
                          "bytes=" + str(stat.st_size), flush=True)
            time.sleep(0.5)
        if process.returncode:
            raise subprocess.CalledProcessError(process.returncode, command)
        if not bundle.is_file():
            raise SystemExit("Gerbil compiler did not publish the native bundle")
        shutil.copy2(bundle, output)
    print("NATIVE-BUNDLE-OK " + str(output), flush=True)


if __name__ == "__main__":
    main()
