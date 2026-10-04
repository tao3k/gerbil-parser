#!/usr/bin/env python3
"""Run current-source performance contracts before native compilation."""

import argparse
import hashlib
import os
from pathlib import Path
import subprocess
import sys
import tempfile


SUITES = {
    "lalr": "t/lalr-fixed-point-benchmark-test.ss",
    "incremental": "t/incremental-suffix-benchmark-test.ss",
    "recovery": "t/recovery-frontier-benchmark-test.ss",
    "glr": "t/selective-glr-benchmark-test.ss",
    "hot-path": "t/benchmark-test.ss",
    "gql": "languages/gql/iso-39075-2024/runtime-benchmark-test.ss",
    "opencypher": "t/opencypher-large-query-benchmark-test.ss",
    "languages": "t/versioned-language-benchmark-test.ss",
}


def source_digest(root):
    digest = hashlib.sha256()
    paths = [root / "gerbil.pkg", root / "justfile"]
    for directory in ("src", "language-support", "languages", "t", "scripts"):
        paths.extend(path for path in (root / directory).rglob("*")
                     if path.is_file() and "__pycache__" not in path.parts)
    for path in sorted(paths):
        if path.is_file():
            data = path.read_bytes()
            digest.update(str(path.relative_to(root)).encode() + b"\0")
            digest.update(str(len(data)).encode() + b"\0" + data)
    return "sha256:" + digest.hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--suite", choices=SUITES, action="append")
    parser.add_argument("--timeout", type=float, default=120)
    parser.add_argument("--log-dir", type=Path)
    parser.add_argument("--continue-on-failure", action="store_true")
    parser.add_argument("--list", action="store_true")
    args = parser.parse_args()
    if args.timeout <= 0:
        parser.error("timeout must be positive")
    selected = args.suite or list(SUITES)
    if args.list:
        for name in selected:
            print(f"{name}: {SUITES[name]}")
        return 0

    root = Path(__file__).resolve().parent.parent
    log_dir = (args.log_dir or root / ".data" / "performance-source").resolve()
    log_dir.mkdir(parents=True, exist_ok=True)
    failures = []
    identity = source_digest(root)
    # Source must precede compiled package artifacts, including stale caches.
    # A private overlay also works when the checkout has a different basename.
    with tempfile.TemporaryDirectory(prefix="parser-source-") as directory:
        overlay = Path(directory)
        (overlay / "gerbil-parser").symlink_to(root, target_is_directory=True)
        environment = os.environ.copy()
        environment.update(
            GERBIL_LOADPATH=str(overlay),
            GERBIL_PATH=str(root / ".gerbil"),
            GAMBOPT="max-heap=1G,debug=q",
        )
        print(f"SOURCE ROOT {root}", flush=True)
        print(f"SOURCE DIGEST {identity}", flush=True)
        for name in selected:
            print(f"GATE {name}", flush=True)
            result = subprocess.run(
                [sys.executable, str(root / "scripts" / "run-bounded.py"),
                 "--timeout", str(args.timeout),
                 "--log", str(log_dir / f"{name}.log"),
                 "--require", "^CASE-OK ",
                 "--require", "^MODULE-OK ",
                 "--require", "^HARNESS-OK ",
                 "--require", "^OK$",
                 "--", "gerbil", "test", "-v", "5", SUITES[name]],
                cwd=root, env=environment,
            )
            if source_digest(root) != identity:
                failures.append("source-changed")
                print("SOURCE CHANGED during preflight", flush=True)
                break
            if result.returncode:
                failures.append(name)
                if not args.continue_on_failure:
                    break
        if failures:
            print(f"PREFLIGHT-FAILED {','.join(failures)}", flush=True)
            return 1
        marker = ("PERFORMANCE-SOURCE-OK" if set(selected) == set(SUITES)
                  else "PERFORMANCE-SOURCE-SUBSET-OK")
        print(f"{marker} {','.join(selected)}", flush=True)
        return 0


if __name__ == "__main__":
    sys.exit(main())
