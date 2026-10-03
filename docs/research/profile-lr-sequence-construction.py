#!/usr/bin/env python3
"""Count actual LR construction operations in a disposable native namespace.

These are operation counts, never timing qualification. Production source and
native objects stay untouched. Instrumentation is applied to the exact HEAD
archive, and the generated source and patch are retained in the receipt.
"""
from pathlib import Path
import argparse, hashlib, io, json, os, platform, re, shutil, signal, subprocess, tarfile, threading, time
p = argparse.ArgumentParser(description=__doc__)
p.add_argument('output', type=Path)
p.add_argument('--require-tail-resume', action='store_true', help='Reject full-parse fallback on tail inverse edits')
a = p.parse_args()
root, out = Path.cwd().resolve(), a.output.resolve()
if subprocess.run(['git', 'diff', '--quiet', 'HEAD', '--', 'src', 'languages', 't']).returncode:
    raise SystemExit('Commit runtime and tests before profiling')
out.mkdir(parents=True, exist_ok=True)
if any(out.iterdir()):
    raise SystemExit('Output directory must be empty')
head = subprocess.check_output(['git', 'rev-parse', 'HEAD'], text=True).strip()
study = out/'study'
study.mkdir()
archive = subprocess.check_output(['git', 'archive', head])
with tarfile.open(fileobj=io.BytesIO(archive)) as tf:
    tf.extractall(study, filter='data')
# Dereference symlinks: no mutable native output can reach another namespace.
shutil.copytree(root/'.gerbil/lib', study/'.gerbil/lib', symlinks=False)
source = study/'src/runtime/event-program.ss'
original = source.read_text()
s = original.replace('(export event-program-value?', '(export event-construction-profile event-construction-reset! event-program-value?', 1)
names = ['append-calls', 'empty-left', 'empty-right', 'singleton-appends', 'multi-concats',
         'normalized-buffers', 'normalized-blocks', 'block-copies', 'copied-leaf-references',
         'branch-allocations', 'carry-merges', 'field-allocations', 'node-allocations',
         'fragment-allocations', 'relocation-calls']
profile = '''\n(def +construction-counts+ (make-vector 15 0))
(def (count! index (amount 1))
  (vector-set! +construction-counts+ index (+ amount (vector-ref +construction-counts+ index))))
(def (event-construction-reset!) (vector-fill! +construction-counts+ 0))
(def (event-construction-profile)
  (map (lambda (key index) (cons key (vector-ref +construction-counts+ index)))
       '(%s) (iota 15)))
''' % ' '.join(names)
s = s.replace('(defstruct event-code-branch', profile+'\n(defstruct event-code-branch', 1)
patches = {
 '(def (event-program-append left right)\n': '(def (event-program-append left right)\n  (count! 0)\n',
 '((or (not left) (null? left)) right)\n        ((or (not right) (null? right)) left)\n        ((= (event-program-sequence-arity right) 1) (event-snoc left right))\n        (else (event-rope-join (event-append-tree left) (event-append-tree right)))':
 '((or (not left) (null? left)) (count! 1) right)\n        ((or (not right) (null? right)) (count! 2) left)\n        ((= (event-program-sequence-arity right) 1) (count! 3) (event-snoc left right))\n        (else (count! 4) (event-rope-join (event-append-tree left) (event-append-tree right)))',
 '(let (tree (event-code-append-tail code))': '(let (tree (event-code-append-tail code))\n      (count! 5) (count! 6 (length (event-code-append-blocks code)))',
 '    (copy-block left left-width 0)': '    (count! 7) (count! 8 (+ left-width right-width))\n    (copy-block left left-width 0)',
 '(def (event-branch left right)\n': '(def (event-branch left right)\n  (count! 9)\n',
 '    (event-append-block (cdr blocks) (event-branch (car blocks) block))':
 '    (begin (count! 10) (event-append-block (cdr blocks) (event-branch (car blocks) block)))',
 '(def (event-program-field name start end body)\n': '(def (event-program-field name start end body)\n  (count! 11)\n',
 '(def (event-program-node-value kind start end body)\n': '(def (event-program-node-value kind start end body)\n  (count! 12)\n',
 '(def (event-program-fragment-value start end body)\n': '(def (event-program-fragment-value start end body)\n  (count! 13)\n',
 '(def (event-program-relocate code delta (moved? #t))\n': '(def (event-program-relocate code delta (moved? #t))\n  (count! 14)\n',
}
for before, after in patches.items():
    if s.count(before) != 1:
        raise SystemExit('Instrumentation seam changed: '+before)
    s = s.replace(before, after, 1)
source.write_text(s)
import difflib
(out/'instrumentation.patch').write_text(''.join(difflib.unified_diff(original.splitlines(True), s.splitlines(True), fromfile='event-program.ss', tofile='instrumented-event-program.ss')))
harness = study/'profile.ss'
harness.write_text('''(import :gerbil-parser/src/runtime/event-program
 (only-in :gerbil-parser/src/runtime/lr-parser current-lr-event-program-enabled?)
 (only-in :gerbil-parser/src/runtime/incremental make-incremental-session parse-incremental-session
  make-edit apply-edit incremental-session-artifact incremental-session-project-artifact)
 (only-in :gerbil-parser/languages/hcl/v2-24/parser hcl-v2-24-parser parse-hcl-v2-24))
(def (verify session source)
 (unless (and (equal? (incremental-session-artifact session)
                     (parameterize ((current-lr-event-program-enabled? #f)) (parse-hcl-v2-24 source)))
              (equal? (incremental-session-artifact session) (incremental-session-project-artifact session)))
  (error "instrumented LR output changed")))
(def (emit units stage counts receipt)
 (write (append (list (cons 'workload 'lr-construction-profile) (cons 'input-units units)
                     (cons 'stage stage) (cons 'complete-artifact-equal? #t)) counts receipt))
 (newline) (force-output))
(def (main . args)
 (parameterize ((current-lr-event-program-enabled? #t))
  (for-each (lambda (units)
   (let (source (string-join (make-list units "a = 1\\n") ""))
    (event-construction-reset!)
    (let* ((session (make-incremental-session hcl-v2-24-parser source #t))
           (counts (event-construction-profile)))
     (verify session source) (emit units 'initial counts '())
     (for-each (lambda (location)
      (for-each (lambda (op)
       (let* ((index (case location ((first) 0) ((middle) (quotient units 2)) (else (- units 1))))
              (edit (make-edit (* 6 index) (if (eq? op 'delete) 6 0) (if (eq? op 'insert) "b = 2\\n" "")))
              (inverse (make-edit (* 6 index) (if (eq? op 'insert) 6 0) (if (eq? op 'delete) "a = 1\\n" ""))))
        (event-construction-reset!)
        (let-values (((next receipt) (parse-incremental-session session edit)))
         (let (counts (event-construction-profile))
          (verify next (apply-edit source edit))
          (emit units (string->symbol (string-append (symbol->string location) "-" (symbol->string op))) counts receipt))
         (event-construction-reset!)
         (let-values (((restored receipt) (parse-incremental-session next inverse)))
          (let (counts (event-construction-profile))
           (verify restored source)
           (emit units (string->symbol (string-append (symbol->string location) "-" (symbol->string op) "-inverse")) counts receipt))))))
       '(insert delete))) '(first middle last))))) '(400 800)))
 (displayln "LR-CONSTRUCTION-PROFILE-ALL-OK"))
(export main)
''')
launcher = Path(shutil.which('gxi')).read_text()
mapping = dict(re.findall(r'export (GERBIL_(?:HOME|PREFIX))="([^"$]+)"', launcher))
home = mapping['GERBIL_HOME']
env = dict(os.environ, **mapping, GERBIL_PATH=str(study/'.gerbil'),
           GAMBOPT=f'max-heap=1G,debug=q,~~={home},~~bin={home}/bin,~~lib={home}/lib')
# Recompile callers of inline constructors, not just the defining module.
# Otherwise old .ssxi expansions silently bypass allocation counters.
batches = [
 ['src/runtime/event-program.ss', 'src/runtime/event-reduce.ss', 'src/runtime/funcs.ss'],
 ['src/runtime/lr-parser.ss', 'src/runtime/artifact.ss', 'src/runtime/parser.ss', 'src/runtime/incremental.ss'],
 ['languages/hcl/v2-24/direct-step.ss'],
]
receipt = dict(head=head, schema='gerbil-parser.lr-construction-profile.v1',
               scope='Instrumented operation counts only; no CPU performance claim',
               source_sha256=hashlib.sha256(original.encode()).hexdigest(),
               instrumented_sha256=hashlib.sha256(s.encode()).hexdigest(),
               harness_sha256=hashlib.sha256(harness.read_bytes()).hexdigest(),
               builds=[], build_batch_seconds=180, complete=False)
owner_native = root/'.gerbil/lib/gerbil-parser/src/runtime'
before_native = {f.name: hashlib.sha256(f.read_bytes()).hexdigest()
                 for f in owner_native.glob('event-program*') if f.is_file()}
for index, sources in enumerate(batches, 1):
    print('BUILD-ISOLATED-INSTRUMENTATION', index, flush=True)
    started = time.monotonic()
    command = ['gxc', '-O', *sources]
    log_path = out/f'build-{index}.log'
    with log_path.open('w') as log:
        build = subprocess.Popen(command, cwd=study, env=env,
                                 stdout=log, stderr=subprocess.STDOUT, start_new_session=True)
        try:
            status = build.wait(timeout=180)
        except subprocess.TimeoutExpired:
            os.killpg(build.pid, signal.SIGKILL)
            status = build.wait()
    receipt['builds'].append(dict(command=command, exit=status,
                                 wall_seconds=time.monotonic()-started,
                                 log_sha256=hashlib.sha256(log_path.read_bytes()).hexdigest()))
    (out/'metadata.json').write_text(json.dumps(receipt, indent=2)+'\n')
    if status:
        raise SystemExit('Isolated instrumentation build failed')
    print('BUILD-ISOLATED-INSTRUMENTATION-OK', index, flush=True)
if platform.system() == 'Darwin':
    env['DYLD_PRINT_APIS'] = '1'
command = [str(Path(home)/'bin/gxi'), 't/fixtures/benchmark-progress.ss', 'profile.ss']
activity, started, reason = [time.monotonic()], time.monotonic(), [None]
proc = subprocess.Popen(command, cwd=study, env=env, stdout=subprocess.PIPE,
                        stderr=subprocess.STDOUT, text=True, start_new_session=True)
def guard():
    while proc.poll() is None:
        if time.monotonic()-activity[0] > 5 or time.monotonic()-started > 180:
            reason[0] = 'inactivity' if time.monotonic()-activity[0] > 5 else 'batch-timeout'
            try:
                os.killpg(proc.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            return
        time.sleep(.1)
monitor = threading.Thread(target=guard, daemon=True)
monitor.start()
with (out/'profile.log').open('w') as log:
    for line in proc.stdout:
        activity[0] = time.monotonic()
        log.write(line)
        log.flush()
        if line.startswith(('((workload .', 'LR-CONSTRUCTION-PROFILE-ALL-OK')):
            print('PROFILE-STAGE-OK' if line.startswith('((workload .') else line.strip(), flush=True)
status = proc.wait()
monitor.join()
lines = (out/'profile.log').read_text().splitlines()
rows = [dict(re.findall(r'\(([^ ()]+) \. ([^()]*)\)', line)) for line in lines if line.startswith('((workload .')]
expected = {'initial'} | {f'{loc}-{op}{suffix}' for loc in ['first','middle','last']
                         for op in ['insert','delete'] for suffix in ['', '-inverse']}
complete = (status == 0 and not reason[0] and len(rows) == 26
            and {(row['input-units'], row['stage']) for row in rows} ==
                {(str(units), stage) for units in [400,800] for stage in expected}
            and all(row['complete-artifact-equal?'] == '#t' for row in rows)
            and all(int(row['append-calls']) == sum(int(row[k]) for k in
                    ['empty-left','empty-right','singleton-appends','multi-concats']) for row in rows)
            and all(int(row['field-allocations']) > 0 and int(row['node-allocations']) > 0
                    for row in rows if row['stage'] == 'initial')
            and (not a.require_tail_resume or all(
                    row.get('freshFallback?') != '#t'
                    and int(row['checkpointReusedShiftCount']) > 0
                    and int(row['remainingSignificantTokenCount']) < int(row['input-units'])
                    for row in rows if row['stage'] in ['last-insert-inverse', 'last-delete-inverse']))
            and 'LR-CONSTRUCTION-PROFILE-ALL-OK' in lines
            and not any('*** ERROR' in line for line in lines))
receipt.update(require_tail_resume=a.require_tail_resume, command=command, exit=status, watchdog=reason[0], inactivity_seconds=5,
               batch_seconds=180, rows=rows, complete=complete,
               owner_source_unchanged=(root/'src/runtime/event-program.ss').read_text() == original,
               owner_native_unchanged=before_native == {f.name: hashlib.sha256(f.read_bytes()).hexdigest()
                 for f in owner_native.glob('event-program*') if f.is_file()},
               log_sha256=hashlib.sha256((out/'profile.log').read_bytes()).hexdigest())
(out/'metadata.json').write_text(json.dumps(receipt, indent=2)+'\n')
print('LR-CONSTRUCTION-PROFILE-COMPLETE', complete, 'ROWS', len(rows), flush=True)
raise SystemExit(0 if complete and receipt['owner_source_unchanged'] and receipt['owner_native_unchanged'] else 1)
