#!/usr/bin/env python3
"""Explicit native test selection, bounded concurrency and an aggregate deadline.

Uses existing native products and the upstream disposable test child.
Compile changed dependencies before this command.
Functional concurrency is supported; benchmark modules require --workers 1.
"""
from pathlib import Path
import argparse
from concurrent.futures import ThreadPoolExecutor, as_completed
import hashlib
import json
import os
import platform
import re
import shutil
import signal
import subprocess
import threading
import time


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def summarize(lines, module, status, stopped):
    started = passed = 0
    active = False
    for line in lines:
        if line.startswith('MODULE '):
            active = line.removeprefix('MODULE ') == module
        elif active and line.startswith('CASE '):
            started += 1
        elif active and line.startswith('CASE-OK '):
            passed += 1
        elif line.startswith('MODULE-OK '):
            active = False
    errors = any(marker in line for line in lines
                 for marker in ('ERROR CHECK', 'ERROR HARNESS', '*** ERROR'))
    ok = (status == 0 and stopped is None and started > 0 and started == passed
          and f'MODULE-OK {module}' in lines
          and any(line.startswith('HARNESS-OK ') for line in lines)
          and 'OK' in lines and 'HARNESS-RETURN status=0' in lines
          and 'FINAL-GC-OK' in lines and 'NATIVE-SUITE-OK modules=1' in lines
          and not errors)
    return dict(module=module, started=started, passed=passed, exit=status,
                stopped=stopped, ok=ok)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('output', type=Path)
    parser.add_argument('--module', action='append', required=True)
    parser.add_argument('--workers', type=int, choices=(1, 2, 3), default=2)
    parser.add_argument('--wall-seconds', type=int, default=90)
    args = parser.parse_args()
    if not 1 <= args.wall_seconds <= 90:
        parser.error('aggregate test budget must be between 1 and 90 seconds')
    root = Path.cwd().resolve()
    modules = args.module
    if len(set(modules)) != len(modules):
        parser.error('duplicate module selection')
    for module in modules:
        path = root / module
        if (not path.is_file() or path.is_symlink()
                or not path.resolve().is_relative_to(root)
                or not module.endswith('-test.ss')):
            parser.error('expected a repository test module: ' + module)
        # Direct performance owners are serialized. Imported benchmark owners
        # must also be selected with one worker by the caller.
        if args.workers > 1 and ('benchmark-contract-run' in path.read_text()
                                 or 'benchmark' in module):
            parser.error('performance modules require --workers 1: ' + module)
    if subprocess.run(['git', 'diff', '--quiet', 'HEAD', '--',
                       'src', 'languages', 't']).returncode:
        parser.error('commit source/tests before qualification')
    out = args.output.resolve()
    out.mkdir(parents=True, exist_ok=True)
    if any(out.iterdir()):
        parser.error('output directory must be empty')
    executor = shutil.which('gxi')
    if not executor:
        parser.error('gxi is unavailable')
    env = dict(os.environ, GERBIL_PATH=str(root / '.gerbil'),
               GAMBOPT='max-heap=1G,debug=q')
    if platform.system() == 'Darwin':
        launcher = Path(executor).read_bytes()[:8192].decode(errors='ignore')
        mappings = dict(re.findall(r'export (GERBIL_(?:HOME|PREFIX))="([^"$]+)"', launcher))
        if 'GERBIL_HOME' in mappings:
            env.update(mappings)
            home = mappings['GERBIL_HOME']
            executor = str(Path(home) / 'bin/gxi')
            env['GAMBOPT'] += f',~~={home},~~bin={home}/bin,~~lib={home}/lib'
        if Path(executor).read_bytes()[:2] == b'#!':
            parser.error('Darwin native progress requires a native executor')
        env['DYLD_PRINT_APIS'] = '1'
    receipt = dict(schema='gerbil-parser.focused-native.v1',
                   head=subprocess.check_output(['git', 'rev-parse', 'HEAD'], text=True).strip(),
                   workers=args.workers, aggregate_seconds=args.wall_seconds,
                   inactivity_seconds=5, executor=executor,
                   executor_sha256=digest(Path(executor)),
                   fixture_sha256={str(p.relative_to(root)): digest(p) for p in [root / 't/fixtures/tla-sany-differential' / name for name in ('native-suite.ss', 'preload.ss', 'watch.ss', 'exit-child-process.ss')]},
                   module_sha256={m: digest(root / m) for m in modules},
                   scope='Selected modules with existing native products and upstream child exit; no rebuild, full-suite or library-unload qualification',
                   jobs=[], complete=False)
    start = time.monotonic()
    deadline = start + args.wall_seconds
    print_lock = threading.Lock()

    def run(index, module):
        if time.monotonic() >= deadline:
            return dict(module=module, started=0, passed=0, exit=None,
                        stopped='aggregate-deadline-before-start', ok=False)
        command = [executor, '-:max-heap=1G,debug=q',
                   't/fixtures/tla-sany-differential/native-suite.ss', module]
        with print_lock:
            print('START', index, module, flush=True)
        proc = subprocess.Popen(command, cwd=root, env=env,
                                stdin=subprocess.DEVNULL, stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT, text=True, start_new_session=True)
        activity = [time.monotonic()]
        stopped = [None]
        def guard():
            while proc.poll() is None:
                now = time.monotonic()
                if now >= deadline or now - activity[0] > 5:
                    stopped[0] = 'aggregate-deadline' if now >= deadline else 'inactivity'
                    try:
                        os.killpg(proc.pid, signal.SIGKILL)
                    except ProcessLookupError:
                        pass
                    return
                time.sleep(.05)
        monitor = threading.Thread(target=guard, daemon=True)
        monitor.start()
        log = out / f'job-{index}.log'
        with log.open('w') as output:
            for line in proc.stdout:
                activity[0] = time.monotonic()
                output.write(line)
                output.flush()
                if line.startswith(('CASE-OK ', 'MODULE-OK ', 'HARNESS-OK ')):
                    with print_lock:
                        print(f'[{index}] {line}', end='', flush=True)
        status = proc.wait()
        monitor.join()
        result = summarize(log.read_text().splitlines(), module, status, stopped[0])
        result.update(command=command, log=log.name, log_sha256=digest(log))
        with print_lock:
            print('FINISH', index, module, result['ok'], result['stopped'], flush=True)
        return result

    with ThreadPoolExecutor(max_workers=args.workers) as pool:
        futures = {pool.submit(run, i + 1, m): i for i, m in enumerate(modules)}
        results = {}
        for future in as_completed(futures):
            results[futures[future]] = future.result()
            receipt['jobs'] = [results[i] for i in sorted(results)]
            (out / 'metadata.json').write_text(json.dumps(receipt, indent=2) + '\n')
    receipt['wall_seconds'] = time.monotonic() - start
    receipt['complete'] = all(job['ok'] for job in receipt['jobs'])
    (out / 'metadata.json').write_text(json.dumps(receipt, indent=2) + '\n')
    print('FOCUSED-COMPLETE', receipt['complete'], flush=True)
    return 0 if receipt['complete'] else 1


if __name__ == '__main__':
    raise SystemExit(main())
