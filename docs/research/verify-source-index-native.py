#!/usr/bin/env python3
"""Bounded native module qualification with real progress and sealed logs."""
from pathlib import Path
import argparse, hashlib, json, os, platform, re, shutil, signal, subprocess, threading, time

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('output', type=Path)
parser.add_argument('--resume', action='store_true', help='Retry a final fully checked watchdog failure once, retaining its failed log')
args = parser.parse_args()
root, out = Path.cwd(), args.output.resolve()
head = subprocess.check_output(['git', 'rev-parse', 'HEAD'], text=True).strip()
if subprocess.run(['git', 'diff', '--quiet', 'HEAD', '--', 'src', 'languages', 't']).returncode:
    raise SystemExit('Commit runtime and tests before qualification')
out.mkdir(parents=True, exist_ok=True)
if any(out.iterdir()) and not args.resume:
    raise SystemExit('Output directory must be empty')
modules = sorted(str(p) for p in Path('t').glob('*-test.ss'))
modules += sorted(str(p) for p in Path('languages').rglob('*test.ss'))
env = dict(os.environ, GERBIL_PATH=str(root/'.gerbil'),
           GAMBOPT='max-heap=1G,debug=q', GERBIL_PARSER_LR_TRACE='1')
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
fixture_sources = ['t/fixtures/lr1-construction.ss', 't/line-structure-assertions.ss']
fixture_build = None
if not args.resume:
    command = ['gxc', '-O', *fixture_sources]
    print('BUILD-TEST-FIXTURE', *command, flush=True)
    log = out/'fixture-build.log'
    with log.open('w') as output:
        status = subprocess.run(command, env=env, stdout=output, stderr=subprocess.STDOUT,
                                timeout=30).returncode
    fixture_build = {'command': command, 'exit': status,
                     'sha256': hashlib.sha256(log.read_bytes()).hexdigest()}
    if status:
        raise SystemExit('Native fixture build failed: '+str(status))
    print('BUILD-TEST-FIXTURE-OK', flush=True)
receipt = {'head': head, 'modules': len(modules), 'inactivity_seconds': 5,
           'batch_seconds': 180, 'executor': executor, 'dyld_api_progress': unload_progress,
           'executor_sha256': hashlib.sha256(Path(executor).read_bytes()).hexdigest(), 'fixture_sha256': {path.name: hashlib.sha256(path.read_bytes()).hexdigest() for path in (root/'t/fixtures').glob('*progress.ss')}, 'batches': [], 'fixture_build': fixture_build}
for name in fixture_sources:
    receipt['fixture_sha256'][name] = hashlib.sha256((root/name).read_bytes()).hexdigest()
if args.resume:
    previous = json.loads((out/'metadata.json').read_text())
    for key in ['head', 'modules', 'fixture_sha256', 'executor', 'executor_sha256', 'dyld_api_progress']:
        if previous.get(key) != receipt[key]:
            raise SystemExit('Resume pin changed: '+key)
    if previous.get('complete') or not previous['batches']:
        raise SystemExit('Resume requires a failed final batch')
    for index, batch in enumerate(previous['batches']):
        expected = modules[index*3:index*3+3]
        data = (out/f"batch-{index+1}.log").read_bytes()
        if batch['modules'] != expected or hashlib.sha256(data).hexdigest() != batch['sha256']:
            raise SystemExit('Resume batch or log changed')
        if index < len(previous['batches'])-1 and not batch['ok']:
            raise SystemExit('Earlier batch is unqualified')
    failed = previous['batches'][-1]
    lines = data.decode().splitlines()
    if not (failed['watchdog'] and failed['exit'] == -9 and failed['passed'] == len(expected)
            and 'OK' in lines and any(line.startswith('HARNESS-OK ') for line in lines)
            and not any(marker in line for line in lines for marker in ['ERROR CHECK', 'ERROR HARNESS', '*** ERROR'])):
        raise SystemExit('Only fully checked final watchdog failures can be resumed')
    failures = previous.setdefault('failed_attempts', [])
    if any(batch['batch'] == failed['batch'] for batch in failures):
        raise SystemExit('One fresh retry per batch is the bound')
    retained = f"batch-{failed['batch']}.failed-1.log"
    (out/f"batch-{failed['batch']}.log").rename(out/retained)
    failures.append(dict(failed, log_name=retained, qualification='failed-output-complete-watchdog'))
    previous['batches'].pop()
    receipt = previous
    (out/'metadata.json').write_text(json.dumps(receipt, indent=2)+'\n')
    print('RETRY-FAILED-RETAINED', retained, flush=True)
for offset in range(3*len(receipt['batches']), len(modules), 3):
    batch, batch_id = modules[offset:offset+3], offset//3+1
    print('BATCH', batch_id, *batch, flush=True)
    activity, quiet = [time.monotonic()], threading.Event()
    proc = subprocess.Popen([executor, '-:max-heap=1G,debug=q', 't/fixtures/native-progress.ss',
                             '-v', '6', *batch], env=env, stdin=subprocess.DEVNULL,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                            text=True, start_new_session=True)
    def guard():
        started = time.monotonic()
        while proc.poll() is None:
            if time.monotonic()-activity[0] > 5 or time.monotonic()-started > 180:
                quiet.set()
                try:
                    os.killpg(proc.pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
                return
            time.sleep(.1)
    monitor = threading.Thread(target=guard, daemon=True)
    monitor.start()
    log = out/f'batch-{batch_id}.log'
    with log.open('w') as output:
        for line in proc.stdout:
            activity[0] = time.monotonic()
            output.write(line)
            output.flush()
            if line.startswith(('CASE-OK ', 'MODULE-OK ', 'HARNESS-OK ')) or line.strip() == 'OK':
                print(line, end='', flush=True)
    status = proc.wait()
    monitor.join()
    lines = log.read_text().splitlines()
    passed = sum(line.startswith('MODULE-OK ') for line in lines)
    ok = (not quiet.is_set() and status == 0 and passed == len(batch)
          and any(line.startswith('HARNESS-OK ') for line in lines) and 'OK' in lines
          and not any(marker in line for line in lines
                      for marker in ['ERROR CHECK', 'ERROR HARNESS', '*** ERROR']))
    receipt['batches'].append({'batch': batch_id, 'modules': batch, 'passed': passed,
                               'cases': sum(line.startswith('CASE-OK ') for line in lines),
                               'exit': status, 'watchdog': quiet.is_set(), 'ok': ok,
                               'sha256': hashlib.sha256(log.read_bytes()).hexdigest()})
    (out/'metadata.json').write_text(json.dumps(receipt, indent=2)+'\n')
    if not ok:
        raise SystemExit('BATCH-FAIL '+str(batch_id))
    print('BATCH-OK', batch_id, 'TOTAL', offset+passed, flush=True)
receipt['complete'] = True
(out/'metadata.json').write_text(json.dumps(receipt, indent=2)+'\n')
print('NATIVE-ALL-OK', len(modules), flush=True)
