#!/usr/bin/env python3
"""Isolate canonical publication with equivalent roots and identical source/token binding."""
from pathlib import Path
import argparse, hashlib, json, os, platform, re, shutil, signal, subprocess, threading, time
p = argparse.ArgumentParser(description=__doc__)
p.add_argument('output', type=Path)
a = p.parse_args()
root, out = Path.cwd().resolve(), a.output.resolve()
harness = 't/benchmarks/semantic-publication/benchmark.ss'
if subprocess.run(['git', 'diff', '--quiet', 'HEAD', '--', 'src', 'languages', harness]).returncode:
    raise SystemExit('Commit runtime and publication harness before measuring')
out.mkdir(parents=True, exist_ok=True)
if any(out.iterdir()):
    raise SystemExit('Output directory must be empty')
executor = shutil.which('gxi')
if not executor:
    raise SystemExit('Native gxi is required')
env = dict(os.environ, GERBIL_PATH=str(root/'.gerbil'), GAMBOPT='max-heap=1G,debug=q')
if platform.system() == 'Darwin':
    mapping = dict(re.findall(r'export (GERBIL_(?:HOME|PREFIX))="([^"$]+)"', Path(executor).read_text()))
    home = mapping['GERBIL_HOME']
    env.update(mapping)
    executor = str(Path(home)/'bin/gxi')
    env['GAMBOPT'] += f',~~={home},~~bin={home}/bin,~~lib={home}/lib'
    env['DYLD_PRINT_APIS'] = '1'
command = [executor, 't/fixtures/benchmark-progress.ss', harness]
head = subprocess.check_output(['git', 'rev-parse', 'HEAD'], text=True).strip()
activity, start, reason = [time.monotonic()], time.monotonic(), [None]
proc = subprocess.Popen(command, env=env, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                        text=True, start_new_session=True)
def guard():
    while proc.poll() is None:
        if time.monotonic()-activity[0] > 5 or time.monotonic()-start > 180:
            reason[0] = 'inactivity' if time.monotonic()-activity[0] > 5 else 'batch-timeout'
            try:
                os.killpg(proc.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            return
        time.sleep(.1)
monitor = threading.Thread(target=guard, daemon=True)
monitor.start()
log = out/'publication.log'
with log.open('w') as output:
    for line in proc.stdout:
        activity[0] = time.monotonic()
        output.write(line)
        output.flush()
        if line.startswith(('((workload .', 'SEMANTIC-PUBLICATION-ALL-OK')):
            print(line, end='', flush=True)
status = proc.wait()
monitor.join()
lines = log.read_text().splitlines()
rows = [dict(re.findall(r'\(([^ ()]+) \. ([^()]*)\)', line))
        for line in lines if line.startswith('((workload .')]
pairs = {}
for row in rows:
    key = (row['family'], row['input-units'], row['order'])
    modes = pairs.setdefault(key, {})
    if row['mode'] in modes:
        raise SystemExit('Duplicate publication row')
    modes[row['mode']] = row
complete = (status == 0 and not reason[0] and len(rows) == 16 and len(pairs) == 8
            and 'SEMANTIC-PUBLICATION-ALL-OK' in lines
            and all(set(modes) == {'canonical', 'unified-event'} for modes in pairs.values())
            and all(row['samples'] == '20' and row['complete-artifact-equal?'] == '#t' for row in rows)
            and not any('*** ERROR' in line for line in lines))
receipt = dict(schema='gerbil-parser.semantic-publication.v1', head=head,
               command=command, samples=20, rows=len(rows), complete=complete,
               exit=status, watchdog=reason[0], inactivity_seconds=5, batch_seconds=180,
               executor_sha256=hashlib.sha256(Path(executor).read_bytes()).hexdigest(),
               harness_sha256=hashlib.sha256((root/harness).read_bytes()).hexdigest(),
               log_sha256=hashlib.sha256(log.read_bytes()).hexdigest(),
               scope='Only complete artifact publication; parser/scanner/retention costs excluded',
               comparisons=[dict(family=key[0], input_units=int(key[1]), order=int(key[2]),
                                 canonical_ms=float(modes['canonical']['cpu-median-ms']),
                                 unified_ms=float(modes['unified-event']['cpu-median-ms']),
                                 change_pct=100*(float(modes['unified-event']['cpu-median-ms'])/
                                                 float(modes['canonical']['cpu-median-ms'])-1))
                            for key, modes in pairs.items() if set(modes) == {'canonical', 'unified-event'}])
(out/'metadata.json').write_text(json.dumps(receipt, indent=2)+'\n')
print('PUBLICATION-COMPLETE', complete, 'ROWS', len(rows), flush=True)
raise SystemExit(0 if complete else 1)
