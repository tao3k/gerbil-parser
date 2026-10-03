#!/usr/bin/env python3
"""Bounded native module qualification with real progress and sealed logs."""
from pathlib import Path
import argparse, hashlib, json, os, signal, subprocess, threading, time

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('output', type=Path)
args = parser.parse_args()
root, out = Path.cwd(), args.output.resolve()
head = subprocess.check_output(['git', 'rev-parse', 'HEAD'], text=True).strip()
if subprocess.run(['git', 'diff', '--quiet', 'HEAD', '--', 'src', 'languages', 't']).returncode:
    raise SystemExit('Commit runtime and tests before qualification')
out.mkdir(parents=True, exist_ok=True)
if any(out.iterdir()):
    raise SystemExit('Output directory must be empty')
modules = sorted(str(p) for p in Path('t').glob('*-test.ss'))
modules += sorted(str(p) for p in Path('languages').rglob('*test.ss'))
env = dict(os.environ, GERBIL_PATH=str(root/'.gerbil'),
           GAMBOPT='max-heap=1G,debug=q', GERBIL_PARSER_LR_TRACE='1')
receipt = {'head': head, 'modules': len(modules), 'inactivity_seconds': 5,
           'batch_seconds': 180, 'batches': []}
for offset in range(0, len(modules), 3):
    batch, batch_id = modules[offset:offset+3], offset//3+1
    print('BATCH', batch_id, *batch, flush=True)
    activity, quiet = [time.monotonic()], threading.Event()
    proc = subprocess.Popen(['gxi', '-:max-heap=1G,debug=q', 't/fixtures/native-progress.ss',
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
