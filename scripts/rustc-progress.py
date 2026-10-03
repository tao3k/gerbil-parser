#!/usr/bin/env python3
"""Forward rustc unchanged and report changes to that invocation's output files."""

import os
from pathlib import Path
import subprocess
import sys


def output_identity(arguments):
    try:
        directory = Path(arguments[arguments.index("--out-dir") + 1])
        crate = arguments[arguments.index("--crate-name") + 1]
    except (ValueError, IndexError):
        return None
    suffix = ""
    for argument in arguments:
        if argument.startswith("extra-filename="):
            suffix = argument.removeprefix("extra-filename=")
    # Cargo gives each crate/configuration an exact output suffix. Do not count
    # writes from other Cargo invocations or other crates as this child's work.
    if not suffix:
        return None
    return directory, (crate + suffix, "lib" + crate + suffix)


def snapshot(identity):
    if identity is None:
        return {}
    directory, prefixes = identity
    result = {}
    try:
        for path in directory.iterdir():
            if not path.name.startswith(prefixes):
                continue
            try:
                info = path.stat()
                if path.is_file():
                    result[path.name] = (info.st_size, info.st_mtime_ns)
            except FileNotFoundError:
                pass
    except FileNotFoundError:
        pass
    return result


def main():
    command = sys.argv[1:]
    if not command:
        raise SystemExit("expected rustc and its arguments")
    identity = output_identity(command[1:])
    previous = snapshot(identity)
    # Preserve the Cargo jobserver descriptors exactly as an exec would.
    process = subprocess.Popen(command, close_fds=False)
    try:
        while True:
            try:
                status = process.wait(timeout=1)
            except subprocess.TimeoutExpired:
                status = None
            current = snapshot(identity)
            changed = {name: values for name, values in current.items()
                       if values[0] > 0 and previous.get(name) != values}
            if changed:
                print("RUSTC-OUTPUT " + ", ".join(
                    f"{name} bytes={values[0]}" for name, values in sorted(changed.items())),
                    file=sys.stderr, flush=True)
            previous = current
            if status is not None:
                return status if status >= 0 else 128 - status
    finally:
        if process.poll() is None:
            process.terminate()
            process.wait()


if __name__ == "__main__":
    sys.exit(main())
