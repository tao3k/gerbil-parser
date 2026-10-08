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
import resource
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
    compiler = shlex.split(subprocess.check_output(
        [str(Path(home) / "bin/gambuild-C"), "C_COMPILER"],
        text=True, timeout=5).strip())
    if len(compiler) != 1:
        raise SystemExit("SDK C_COMPILER must name one compiler executable")
    print("SHARED-LIBRARY-SDK-COMPILER", compiler[0], flush=True)
    with tempfile.TemporaryDirectory(prefix="gerbil-shared-library-") as temporary:
        directory = Path(temporary)
        source = directory / ("bundle" + uuid.uuid4().hex + ".ss")
        source.write_text("(import " + " ".join("(prefix-in :" + module + " language" + str(index) + "-)"
                                    for index, module in enumerate(args.language_module))
                          + ")\n(export main)\n(def (main . _) (void))\n")
        shim = directory / "runtime.o"
        print("SHARED-LIBRARY-RUNTIME-COMPILE", flush=True)
        subprocess.run(compiler + [
            "-std=c11", "-D_POSIX_C_SOURCE=200809L", "-fPIC", "-I" + str(Path(home) / "include"),
            "-I" + str(root / "include"),
            "-DGERBIL_PARSER_LINKER=___LNK_gpembed____exe__",
            "-c", str(root / "include/gerbil-parser/runtime.c"), "-o", str(shim)], check=True)
        # A fixed compiler output basename defines the SDK linker identifier.
        # The source module identity is unique to avoid static-cache collisions.
        bundle = directory / "gpembed"
        if platform.system() == "Darwin":
            linker = "-dynamiclib -Wl,-install_name," + str(output)
        else:
            linker = "-shared -Wl,-z,defs"
        environment = os.environ.copy()
        # Use the installed SDK compiler and its complete optimization/ABI
        # configuration. Ambient CC must not substitute the Scheme compiler.
        environment["GERBIL_GSC"] = str(Path(home) / "bin/gsc")
        environment["GERBIL_GCC"] = compiler[0]
        preload = '(gx#import-module (quote :gerbil/compiler) #f #t)'
        for module in args.language_module:
            preload += ' (gx#import-module (string->symbol ' + '"' + ":" + module + '"' + ') #f #t)'
        preload += ' (void)'
        print("SHARED-LIBRARY-GERBIL-AOT", flush=True)
        command = ["gxi", "-e", preload, str(root / "build-shared-library.ss"), str(source),
            str(bundle), "-save-temps=obj -fPIC -fwrapv -fno-strict-aliasing -foptimize-sibling-calls "
            + "-I" + str(root / "include"), linker + " " + str(shim)]
        # Report actual preprocessing/assembly/object writes. These are real
        # compiler artifacts, not periodic heartbeats or simulated progress.
        observed = {shim: (shim.stat().st_size, shim.stat().st_mtime_ns)}
        started = time.monotonic()
        usage_before = resource.getrusage(resource.RUSAGE_CHILDREN)
        process = subprocess.Popen(command, env=environment)
        while process.poll() is None:
            for artifact in directory.rglob("*"):
                if artifact.suffix not in {".i", ".s", ".o"}:
                    continue
                try:
                    stat = artifact.stat()
                except FileNotFoundError:
                    continue
                state = (stat.st_size, stat.st_mtime_ns)
                if stat.st_size and observed.get(artifact) != state:
                    observed[artifact] = state
                    print("NATIVE-COMPILER-OUTPUT", artifact.relative_to(directory),
                          "bytes=" + str(stat.st_size),
                          "elapsed-s=" + format(time.monotonic() - started, ".2f"), flush=True)
            time.sleep(0.5)
        usage_after = resource.getrusage(resource.RUSAGE_CHILDREN)
        cpu = (usage_after.ru_utime + usage_after.ru_stime
               - usage_before.ru_utime - usage_before.ru_stime)
        print("SHARED-LIBRARY-AOT-COST", "wall-s=" + format(time.monotonic() - started, ".2f"),
              "child-cpu-s=" + format(cpu, ".2f"), flush=True)
        if process.returncode:
            raise subprocess.CalledProcessError(process.returncode, command)
        if not bundle.is_file():
            raise SystemExit("Gerbil compiler did not publish the native bundle")
        shutil.copy2(bundle, output)
    print("SHARED-LIBRARY-OK " + str(output), flush=True)


if __name__ == "__main__":
    main()
