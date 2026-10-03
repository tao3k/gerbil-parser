#!/usr/bin/env python3
"""Matched native source-index controls using the existing session benchmark."""
from pathlib import Path
import argparse, hashlib, json, os, signal, subprocess, threading, time

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('output', type=Path)
parser.add_argument('--samples', type=int, default=20)
args = parser.parse_args()
if not 1 <= args.samples <= 100:
    parser.error('samples must be between 1 and 100')
root = Path.cwd()
head = subprocess.check_output(['git', 'rev-parse', 'HEAD'], text=True).strip()
if subprocess.run(['git', 'diff', '--quiet', 'HEAD', '--', 'src', 'languages',
                   't/benchmarks/incremental-session/benchmark.ss']).returncode:
    raise SystemExit('Commit runtime and benchmark changes before measuring')
out = args.output.resolve()
out.mkdir(parents=True, exist_ok=True)
if any(out.iterdir()):
    raise SystemExit('Output directory must be empty to avoid mixing receipts')
metadata = {'head': head, 'samples': args.samples, 'executor': 'native gxi',
            'inactivity_seconds': 5, 'batch_seconds': 180,
            'harness_sha256': hashlib.sha256((root/'t/benchmarks/incremental-session/benchmark.ss').read_bytes()).hexdigest(),
            'jobs': []}
flat = ['topology-hcl-capture', '400', '800']
nested = ['topology-hcl-nested-capture', '8', '16']
history = ['history-hcl-capture', '400', '800']
history_nested = ['history-hcl-nested-capture', '8', '16']
# Reverse execution order on the second pass to expose ordering/thermal drift.
jobs = []
for family, command, expected in [('flat', flat, 12), ('nested', nested, 12),
                                  ('history', history, 2), ('history-nested', history_nested, 2)]:
    for repeat, order in [(1, ['vector', 'index']), (2, ['index', 'vector'])]:
        for variant in order:
            jobs.append((f'{variant}-{family}-{repeat}',
                         (['no-source-index'] if variant == 'vector' else ['source-index']) + command, expected))
for name, command, expected in jobs:
    print('BENCHMARK', name, 'HEAD', head, flush=True)
    activity = [time.monotonic()]
    failed = threading.Event()
    env = dict(os.environ, GERBIL_PATH=str(root/'.gerbil'),
               GERBIL_PARSER_BENCHMARK_SAMPLES=str(args.samples), GERBIL_PARSER_LR_TRACE='1')
    proc = subprocess.Popen(['gxi', '-:max-heap=1G,debug=q', 't/fixtures/benchmark-progress.ss',
                             't/benchmarks/incremental-session/benchmark.ss', *command],
                            env=env, stdin=subprocess.DEVNULL, stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT, text=True, start_new_session=True)
    def guard():
        started = time.monotonic()
        while proc.poll() is None:
            if time.monotonic()-activity[0] > 5 or time.monotonic()-started > 180:
                failed.set()
                try:
                    os.killpg(proc.pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
                return
            time.sleep(.1)
    monitor = threading.Thread(target=guard, daemon=True)
    monitor.start()
    rows, bad = [], False
    log = out/(name+'.log')
    with log.open('w') as output:
        for line in proc.stdout:
            activity[0] = time.monotonic()
            output.write(line)
            output.flush()
            if line.startswith('((workload .'):
                rows.append(line.rstrip())
                print(line, end='', flush=True)
            if '*** ERROR' in line or 'ERROR in' in line:
                bad = True
    status = proc.wait()
    monitor.join()
    job = {'name': name, 'args': command, 'exit': status, 'watchdog': failed.is_set(),
           'rows': len(rows), 'sha256': hashlib.sha256(log.read_bytes()).hexdigest()}
    metadata['jobs'].append(job)
    (out/'metadata.json').write_text(json.dumps(metadata, indent=2)+'\n')
    if failed.is_set() or status or bad or len(rows) != expected:
        raise SystemExit('FAILED '+json.dumps(job))
    print('BENCHMARK-OK', name, len(rows), flush=True)
metadata['complete'] = True
(out/'metadata.json').write_text(json.dumps(metadata, indent=2)+'\n')
print('MATCHED-ALL-OK', sum(job['rows'] for job in metadata['jobs']), flush=True)
