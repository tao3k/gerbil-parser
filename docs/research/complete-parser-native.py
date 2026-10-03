#!/usr/bin/env python3
"""Complete missing/stale generated native sections after interrupted package builds."""
from pathlib import Path
import argparse, hashlib, json, os, platform, re, shutil, signal, subprocess, threading, time
p = argparse.ArgumentParser(description=__doc__)
p.add_argument('output', type=Path)
a = p.parse_args()
root = Path.cwd().resolve()
base = root/'.gerbil/lib/gerbil-parser'
if base.is_symlink() or not base.is_dir():
    raise SystemExit('Parser output must be an owned directory')
out = a.output.resolve()
out.mkdir(parents=True, exist_ok=True)
if any(out.iterdir()):
    raise SystemExit('Output directory must be empty')
env = dict(os.environ, GERBIL_PATH=str(root/'.gerbil'), GAMBOPT='max-heap=1G,debug=q')
launcher = Path(shutil.which('gxi')).read_bytes()[:8192].decode(errors='ignore')
mapping = dict(re.findall(r'export (GERBIL_(?:HOME|PREFIX))="([^"$]+)"', launcher))
env.update(mapping)
home = mapping.get('GERBIL_HOME')
if not home:
    raise SystemExit('Resolve the native Gerbil home before completing sections')
env['GAMBOPT'] += f',~~={home},~~bin={home}/bin,~~lib={home}/lib'
env['PATH'] = home+'/bin:'+env['PATH']
if platform.system() == 'Darwin':
    env['DYLD_PRINT_APIS'] = '1'

def incomplete():
    result = []
    for source in sorted(base.rglob('*.scm')):
        relative = source.relative_to(base)
        # These intentionally interpreted runtime cache wrappers are not
        # generated native package sections.
        if relative.parts[0] in ['compiled-language-declaration-cache', 'compiled-language-parser-cache']:
            continue
        binaries = [b for b in source.parent.glob(source.stem+'.o*')
                    if b.suffix[2:].isdigit()]
        if not binaries or max(b.stat().st_mtime_ns for b in binaries) < source.stat().st_mtime_ns:
            result.append(source)
    return result

sources = incomplete()
receipt = dict(schema='gerbil-parser.native-section-completion.v1',
               head=subprocess.check_output(['git', 'rev-parse', 'HEAD'], text=True).strip(),
               build_inactivity_seconds=None, build_batch_seconds=180,
               initial_incomplete_sections=len(sources), sections=[], complete=False)
for index, source in enumerate(sources, 1):
    relative = str(source.relative_to(base))
    print('NATIVE-SECTION', index, relative, flush=True)
    # Match compiler/driver gsc-compile-file: target C, plus the package's
    # explicit Darwin linker policy for runtime FFI sections (not phi sections).
    command = [str(Path(home)/'bin/gsc'), '-:max-heap=1G,debug=q', '-verbose', '-target', 'C']
    if platform.system() == 'Darwin' and relative.startswith('src/ffi/') and '-native' in relative and not source.stem.endswith('~1'):
        command += ['-ld-options', '-Wl,-undefined,dynamic_lookup']
    command.append(str(source))
    activity = [time.monotonic()]
    started = time.monotonic()
    reason = [None]
    proc = subprocess.Popen(command, env=env, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                            text=True, start_new_session=True)
    def guard():
        while proc.poll() is None:
            if time.monotonic()-started > 180:
                # Native C generation has silent phases. Bound compilation by
                # wall time; this is not a test/benchmark qualification guard.
                reason[0] = 'batch-timeout'
                try:
                    os.killpg(proc.pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
                return
            time.sleep(.1)
    thread = threading.Thread(target=guard, daemon=True)
    thread.start()
    log = out/f'section-{index}.log'
    with log.open('w') as stream:
        for line in proc.stdout:
            activity[0] = time.monotonic()
            stream.write(line)
    status = proc.wait()
    thread.join()
    receipt['sections'].append(dict(source=relative, source_sha256=hashlib.sha256(source.read_bytes()).hexdigest(),
                                    command=command, exit=status, failure_reason=reason[0],
                                    wall_seconds=time.monotonic()-started,
                                    log_sha256=hashlib.sha256(log.read_bytes()).hexdigest()))
    (out/'metadata.json').write_text(json.dumps(receipt, indent=2)+'\n')
    if status or reason[0]:
        raise SystemExit('Native section compilation failed: '+relative)
    print('NATIVE-SECTION-OK', relative, flush=True)
receipt['remaining_incomplete_sections'] = [str(s.relative_to(base)) for s in incomplete()]
manifest = []
for source in sorted(base.rglob('*.scm')):
    relative = source.relative_to(base)
    if relative.parts[0] in ['compiled-language-declaration-cache', 'compiled-language-parser-cache']:
        continue
    binaries = [b for b in source.parent.glob(source.stem+'.o*') if b.suffix[2:].isdigit()]
    manifest.append(dict(source=str(relative), source_sha256=hashlib.sha256(source.read_bytes()).hexdigest(),
                         binaries=[dict(file=str(b.relative_to(base)), sha256=hashlib.sha256(b.read_bytes()).hexdigest())
                                   for b in sorted(binaries)]))
manifest_path = out/'native-manifest.json'
manifest_path.write_text(json.dumps(manifest, indent=2)+'\n')
receipt['native_manifest_sha256'] = hashlib.sha256(manifest_path.read_bytes()).hexdigest()
receipt['inspected_generated_sections'] = len(manifest)
receipt['native_namespace'] = str(base)
receipt['complete'] = not receipt['remaining_incomplete_sections']
(out/'metadata.json').write_text(json.dumps(receipt, indent=2)+'\n')
if not receipt['complete']:
    raise SystemExit('Generated sections remain incomplete')
print('NATIVE-SECTIONS-COMPLETE', len(sources), flush=True)
