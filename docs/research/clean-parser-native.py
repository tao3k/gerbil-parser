#!/usr/bin/env python3
"""Clean this checkout's generated parser namespace, including Gambit versioned bundles."""
from pathlib import Path
import os, shutil, subprocess

root = Path.cwd().resolve()
if not (root/'src/compiler/machine.ss').is_file() or not (root/'build.ss').is_file():
    raise SystemExit('Run from the gerbil-parser checkout root')
env = dict(os.environ, GERBIL_PATH=str(root/'.gerbil'))
subprocess.run(['gxi', 'build.ss', 'clean'], env=env, check=True)
target = root/'.gerbil/lib/gerbil-parser'
if target.exists():
    if target.is_symlink() or target.resolve() != target:
        raise SystemExit('Refuse to remove a redirected generated namespace')
    if any(target.rglob('.git')):
        raise SystemExit('Refuse to remove a checkout inside the generated namespace')
    count = len(list(target.rglob('*.o[0-9]*')))
    shutil.rmtree(target)
    print('REMOVED-OWN-GENERATED-NAMESPACE', target, 'numbered-bundles', count, flush=True)
else:
    print('OWN-GENERATED-NAMESPACE-ABSENT', target, flush=True)
