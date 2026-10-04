#!/usr/bin/env python3
"""Run a verification command with streamed evidence and process-group timeout."""

import argparse
import os
from pathlib import Path
import re
import selectors
import signal
import subprocess
import sys
import time


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--timeout", type=float, default=60)
    parser.add_argument("--idle-timeout", type=float)
    parser.add_argument("--log", type=Path, required=True)
    parser.add_argument("--require", action="append", default=[])
    parser.add_argument("--full-output", action="store_true",
                        help="preserve complete evidence lines on stdout as well as in the log")
    parser.add_argument("command", nargs=argparse.REMAINDER)
    args = parser.parse_args()
    command = args.command
    if command[:1] == ["--"]:
        command = command[1:]
    if not command or args.timeout <= 0 or (
        args.idle_timeout is not None and args.idle_timeout <= 0
    ):
        parser.error("expected a command and positive timeout values")
    args.log.parent.mkdir(parents=True, exist_ok=True)
    start = last_output = time.monotonic()
    next_progress = start + 5
    output = bytearray()
    pending = bytearray()
    reason = None
    process = subprocess.Popen(
        command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
        start_new_session=True,
    )
    selector = selectors.DefaultSelector()
    selector.register(process.stdout, selectors.EVENT_READ)

    def emit(line):
        text = line.decode("utf-8", errors="replace")
        print(text if args.full_output else text[:1800], flush=True)

    def stop_group():
        # The wrapper may have exited while a compiler/interpreter child lives.
        for sig in (signal.SIGTERM, signal.SIGKILL):
            try:
                os.killpg(process.pid, sig)
            except ProcessLookupError:
                break
            except PermissionError:
                # Some macOS sandboxes permit signaling owned child PIDs but
                # deny group signals. Enumerate only our newly created group.
                try:
                    rows = subprocess.check_output(
                        ["ps", "-axo", "pid=,pgid="], text=True,
                    ).splitlines()
                    members = [int(row.split()[0]) for row in rows
                               if len(row.split()) == 2
                               and int(row.split()[1]) == process.pid]
                except (OSError, subprocess.CalledProcessError):
                    members = [process.pid]
                    print("CLEANUP process-group enumeration unavailable; "
                          "only direct child can be signaled", flush=True)
                for pid in sorted(members, reverse=True):
                    try:
                        os.kill(pid, sig)
                    except ProcessLookupError:
                        pass
            if sig == signal.SIGTERM:
                time.sleep(0.25)

    print(f"START timeout={args.timeout:g}s log={args.log}", flush=True)
    try:
        with args.log.open("wb") as log:
            while selector.get_map():
                now = time.monotonic()
                if now - start >= args.timeout:
                    reason = "total timeout"
                    stop_group()
                    break
                if args.idle_timeout and now - last_output >= args.idle_timeout:
                    reason = "output idle timeout"
                    stop_group()
                    break
                if now >= next_progress:
                    print(f"RUNNING elapsed={now-start:.1f}s idle={now-last_output:.1f}s", flush=True)
                    next_progress = now + 5
                for key, _ in selector.select(timeout=0.1):
                    chunk = os.read(key.fileobj.fileno(), 65536)
                    if not chunk:
                        selector.unregister(key.fileobj)
                        continue
                    last_output = time.monotonic()
                    output.extend(chunk)
                    pending.extend(chunk)
                    log.write(chunk)
                    log.flush()
                    while b"\n" in pending:
                        line, _, rest = pending.partition(b"\n")
                        pending[:] = rest
                        emit(line)
                        if re.match(rb"(?:ERROR\b|\*\*\* ERROR)", line):
                            reason = "error output"
                            stop_group()
                            break
                    if reason:
                        break
                if reason:
                    break
            if pending:
                emit(pending)
        code = process.wait(timeout=2)
    except BaseException:
        stop_group()
        process.wait(timeout=2)
        raise
    finally:
        selector.close()
        process.stdout.close()
    text = output.decode("utf-8", errors="replace")
    missing = [pattern for pattern in args.require
               if not re.search(pattern, text, re.MULTILINE)]
    failed_output = bool(re.search(r"(?:^|\n)(?:ERROR\b|\*\*\* ERROR)", text))
    print(f"RESULT elapsed={time.monotonic()-start:.2f}s exit={code} "
          f"reason={reason or 'completed'} missing={missing} errors={failed_output}", flush=True)
    if reason and reason.endswith("timeout"):
        return 124
    return 1 if reason or code or missing or failed_output else 0


if __name__ == "__main__":
    sys.exit(main())
