#!/usr/bin/env python3
"""Matched native lexical-plan and semantic sequence controls using the existing session benchmark."""
from pathlib import Path
import argparse, hashlib, io, json, os, platform, re, shutil, signal, subprocess, tarfile, threading, time

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('output', type=Path)
parser.add_argument('--baseline', type=Path, help='Compare production defaults with an isolated exact-commit archive using its benchmark for both variants')
parser.add_argument('--event-program', action='store_true', help='Measure direct LR event programs with physical backend assertions')
parser.add_argument('--sequence-fusion', action='store_true', help='Measure semantic sequence fusion with the same edit workloads')
parser.add_argument('--samples', type=int, default=20)
parser.add_argument('--preparation-only', action='store_true', help='Measure scanner-only and proof-inclusive construction controls')
parser.add_argument('--resume', action='store_true', help='Retry only a final output-complete watchdog failure once, retaining its failed receipt')
args = parser.parse_args()
if sum(bool(x) for x in [args.sequence_fusion, args.event_program, args.preparation_only, args.baseline]) > 1:
    parser.error('baseline, sequence-fusion and preparation-only are mutually exclusive')
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
env = dict(os.environ, GERBIL_PATH=str(root/'.gerbil'),
           GAMBOPT='max-heap=1G,debug=q',
           GERBIL_PARSER_BENCHMARK_SAMPLES=str(args.samples), GERBIL_PARSER_LR_TRACE='1')
executor = shutil.which('gxi')
if not executor:
    raise SystemExit('gxi is unavailable')
unload_progress = platform.system() == 'Darwin'
if unload_progress:
    # System-shell launchers discard DYLD_* settings. Preserve their literal
    # Gerbil mappings and invoke the same native binary directly. dyld emits
    # actual dlopen/dlsym/dlclose operations; no timed heartbeat is added.
    launcher = Path(executor).read_bytes()[:8192].decode(errors='ignore')
    mappings = dict(re.findall(r'export (GERBIL_(?:HOME|PREFIX))="([^"$]+)"', launcher))
    if 'GERBIL_HOME' in mappings:
        env.update(mappings)
        home = mappings['GERBIL_HOME']
        executor = str(Path(home)/'bin/gxi')
        env['GAMBOPT'] += f',~~={home},~~bin={home}/bin,~~lib={home}/lib'
    if Path(executor).read_bytes()[:2] == b'#!':
        raise SystemExit('Darwin unload progress requires a native gxi binary')
    env['DYLD_PRINT_APIS'] = '1'
baseline = args.baseline.resolve() if args.baseline else None
baseline_origin = None
harness = root/'t/benchmarks/incremental-session/benchmark.ss'
if baseline:
    baseline_origin = json.loads((baseline/'baseline-origin.json').read_text())
    if baseline_origin.get('repo') != 'tao3k/gerbil-parser':
        raise SystemExit('Unexpected baseline repository identity')
    pinned = baseline_origin['head']
    subprocess.check_call(['git', 'cat-file', '-e', pinned+'^{commit}'])
    tree = subprocess.check_output(['git', 'rev-parse', pinned+'^{tree}'], text=True).strip()
    if baseline_origin.get('source_tree') != tree or baseline_origin.get('build_exit') != 0:
        raise SystemExit('Baseline tree or native build receipt is unqualified')
    archive = subprocess.check_output(['git', 'archive', pinned])
    with tarfile.open(fileobj=io.BytesIO(archive)) as original:
        for entry in original.getmembers():
            if entry.isfile() and (entry.name.startswith(('src/', 'languages/', 't/fixtures/'))
                                   or entry.name in ['gerbil.pkg', 'build.ss']):
                if (baseline/entry.name).read_bytes() != original.extractfile(entry).read():
                    raise SystemExit('Baseline source changed: '+entry.name)
    harness = baseline/'t/benchmarks/incremental-session/benchmark.ss'
    expected = subprocess.check_output(['git', 'show', pinned+':t/benchmarks/incremental-session/benchmark.ss'])
    if harness.read_bytes() != expected or (baseline/'.gerbil/lib/gerbil-parser').is_symlink():
        raise SystemExit('Baseline harness or parser output namespace is not isolated')
    baseline_origin = dict(baseline_origin, harness_sha256=hashlib.sha256(expected).hexdigest(),
                           runtime_module_sha256=hashlib.sha256((baseline/'.gerbil/lib/gerbil-parser/src/runtime/funcs.o1').read_bytes()).hexdigest())
metadata = {'head': head, 'samples': args.samples, 'baseline': baseline_origin, 'sequence_fusion': args.sequence_fusion, 'event_program': args.event_program, 'preparation_only': args.preparation_only, 'executor': executor, 'executor_sha256': hashlib.sha256(Path(executor).read_bytes()).hexdigest(), 'dyld_api_progress': unload_progress,
            'inactivity_seconds': 5, 'batch_seconds': 180,
            'harness_sha256': hashlib.sha256(harness.read_bytes()).hexdigest(),
            'fixture_sha256': {path.name: hashlib.sha256(path.read_bytes()).hexdigest() for path in (root/'t/fixtures').glob('*progress.ss')},
            'jobs': []}
jobs = [('separate-preparation-1', ['no-plan-sharing', 'lexical-plan-prepare'], 2),
        ('shared-preparation-1', ['lexical-plan-prepare'], 2),
        ('shared-preparation-2', ['lexical-plan-prepare'], 2),
        ('separate-preparation-2', ['no-plan-sharing', 'lexical-plan-prepare'], 2)]
if args.sequence_fusion or args.event_program or baseline:
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
    previous, candidate = ('previous', 'candidate') if baseline else ('strict', 'equivalent')
    for repeat, order in [(1, [previous, candidate]), (2, [candidate, previous])]:
        for variant in order:
            flags = ['no-plan-reuse'] if baseline else (
                (['no-event-program'] if variant == 'strict' else ['event-program'])
                if args.event_program else
                (['no-sequence-fusion'] if variant == 'strict' else ['sequence-fusion'])
                if args.sequence_fusion else
                (['no-plan-reuse'] if variant == 'strict' else ['plan-reuse']))
            jobs.append((f'{variant}-{family}-{repeat}', flags + command, expected))
if args.resume:
    previous = json.loads((out/'metadata.json').read_text())
    for key in ['head', 'samples', 'baseline', 'sequence_fusion', 'event_program', 'preparation_only', 'executor', 'executor_sha256', 'dyld_api_progress', 'harness_sha256', 'fixture_sha256']:
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
    job_root = baseline if baseline and name.startswith('previous-') else root
    job_env = dict(env, GERBIL_PATH=str(job_root/'.gerbil'))
    print('BENCHMARK', name, 'HEAD', baseline_origin['head'] if job_root == baseline else head, flush=True)
    activity = [time.monotonic()]
    failed = threading.Event()
    proc = subprocess.Popen([executor, '-:max-heap=1G,debug=q', 't/fixtures/benchmark-progress.ss',
                             str(harness), *command],
                            cwd=job_root, env=job_env, stdin=subprocess.DEVNULL, stdout=subprocess.PIPE,
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
    job = {'name': name, 'cwd': str(job_root), 'args': command, 'exit': status, 'watchdog': failed.is_set(),
           'rows': len(rows), 'sha256': hashlib.sha256(log.read_bytes()).hexdigest()}
    metadata['jobs'].append(job)
    (out/'metadata.json').write_text(json.dumps(metadata, indent=2)+'\n')
    if failed.is_set() or status or bad or len(rows) != expected:
        raise SystemExit('FAILED '+json.dumps(job))
    print('BENCHMARK-OK', name, len(rows), flush=True)
metadata['complete'] = True
(out/'metadata.json').write_text(json.dumps(metadata, indent=2)+'\n')
print('MATCHED-ALL-OK', sum(job['rows'] for job in metadata['jobs']), flush=True)
