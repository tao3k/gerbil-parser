#!/usr/bin/env python3
"""Matched native lexical-plan construction and convergence controls using the existing session benchmark."""
from pathlib import Path
import argparse, hashlib, json, os, signal, subprocess, threading, time

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('output', type=Path)
parser.add_argument('--sequence-fusion', action='store_true', help='Measure semantic sequence fusion with the same edit workloads')
parser.add_argument('--samples', type=int, default=20)
parser.add_argument('--preparation-only', action='store_true', help='Measure scanner-only and proof-inclusive construction controls')
parser.add_argument('--resume', action='store_true', help='Retry only a final output-complete watchdog failure once, retaining its failed receipt')
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
if any(out.iterdir()) and not args.resume:
    raise SystemExit('Output directory must be empty to avoid mixing receipts')
metadata = {'head': head, 'samples': args.samples, 'sequence_fusion': args.sequence_fusion, 'preparation_only': args.preparation_only, 'executor': 'native gxi',
            'inactivity_seconds': 5, 'batch_seconds': 180,
            'harness_sha256': hashlib.sha256((root/'t/benchmarks/incremental-session/benchmark.ss').read_bytes()).hexdigest(),
            'fixture_sha256': {path.name: hashlib.sha256(path.read_bytes()).hexdigest() for path in (root/'t/fixtures').glob('*progress.ss')},
            'jobs': []}
jobs = [('separate-preparation-1', ['no-plan-sharing', 'lexical-plan-prepare'], 2),
        ('shared-preparation-1', ['lexical-plan-prepare'], 2),
        ('shared-preparation-2', ['lexical-plan-prepare'], 2),
        ('separate-preparation-2', ['no-plan-sharing', 'lexical-plan-prepare'], 2)]
if args.sequence_fusion:
    jobs = []
if args.preparation_only:
    jobs += [(name.replace('preparation', 'certificate-preparation'),
              [arg.replace('lexical-plan-prepare', 'lexical-certificate-prepare') for arg in command], expected)
             for name, command, expected in jobs[:4]]
for family, command, expected in ([] if args.preparation_only else [
    ('flat', ['topology-hcl-capture', '400', '800'], 12),
    ('nested', ['topology-hcl-nested-capture', '8', '16'], 12),
    ('history', ['history-hcl-capture', '400', '800'], 2),
    ('history-nested', ['history-hcl-nested-capture', '8', '16'], 2)]):
    for repeat, order in [(1, ['strict', 'equivalent']), (2, ['equivalent', 'strict'])]:
        for variant in order:
            jobs.append((f'{variant}-{family}-{repeat}',
                         ((['no-sequence-fusion'] if variant == 'strict' else ['sequence-fusion']) if args.sequence_fusion else (['no-plan-reuse'] if variant == 'strict' else ['plan-reuse'])) + command, expected))
if args.resume:
    previous = json.loads((out/'metadata.json').read_text())
    for key in ['head', 'samples', 'sequence_fusion', 'preparation_only', 'harness_sha256', 'fixture_sha256']:
        if previous.get(key) != metadata[key]:
            raise SystemExit('Resume pin changed: '+key)
    if previous.get('complete') or not previous['jobs']:
        raise SystemExit('Resume requires an incomplete run with a failed final job')
    for index, job in enumerate(previous['jobs']):
        name, command, expected = jobs[index]
        data = (out/(name+'.log')).read_bytes()
        if job['name'] != name or job['args'] != command or hashlib.sha256(data).hexdigest() != job['sha256']:
            raise SystemExit('Resume job or log changed: '+name)
        if index < len(previous['jobs'])-1 and (job['exit'] or job['watchdog'] or job['rows'] != expected):
            raise SystemExit('Earlier job is unqualified: '+name)
    failed_job = previous['jobs'][-1]
    lines = data.decode().splitlines()
    if not (failed_job['watchdog'] and failed_job['exit'] == -9 and failed_job['rows'] == expected
            and lines[-1].startswith('((workload .')
            and not any(mark in data.decode() for mark in ['*** ERROR', 'ERROR in'])):
        raise SystemExit('Only output-complete final watchdog failures can be resumed')
    failures = previous.setdefault('failed_attempts', [])
    if any(job['name'] == name for job in failures):
        raise SystemExit('One fresh retry per job is the bound')
    retained = name+'.failed-1.log'
    (out/(name+'.log')).rename(out/retained)
    failures.append(dict(failed_job, log_name=retained, qualification='failed-output-complete-watchdog'))
    previous['jobs'].pop()
    metadata = previous
    (out/'metadata.json').write_text(json.dumps(metadata, indent=2)+'\n')
    print('RETRY-FAILED-RETAINED', retained, flush=True)
for name, command, expected in jobs[len(metadata['jobs']):]:
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
