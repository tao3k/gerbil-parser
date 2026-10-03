#!/usr/bin/env python3
"""Seal completed native lexical-plan jobs without admitting interrupted jobs."""
from pathlib import Path
import argparse, hashlib, json

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('input', type=Path)
parser.add_argument('output_stem', type=Path)
parser.add_argument('--partial', action='store_true')
parser.add_argument('--primary', type=Path)
parser.add_argument('--build-log', type=Path)
parser.add_argument('--clean-log', type=Path)
args = parser.parse_args()
metadata = json.loads((args.input/'metadata.json').read_text())
if not metadata.get('complete') and not args.partial:
    parser.error('Incomplete run requires explicit --partial qualification')
metadata['schema'] = 'gerbil-parser.lexical-plan-matched.v1'
metadata['qualification'] = 'completed-jobs-only' if args.partial else 'complete-matched-run'
metadata['build_qualification'] = 'development-observation' if args.partial else 'clean-rebuilt-native'
if not args.partial and (not args.build_log or not args.clean_log):
    parser.error('Final qualification requires clean and build log receipts')
for field, path in [('build_log_sha256', args.build_log), ('clean_log_sha256', args.clean_log)]:
    if path:
        metadata[field] = hashlib.sha256(path.read_bytes()).hexdigest()
if args.primary:
    metadata['primary_source'] = json.loads(args.primary.read_text())
metadata['reproduction_recipe_sha256'] = hashlib.sha256((Path.cwd()/'docs/research/reproduce-lexical-plans.py').read_bytes()).hexdigest()
for job in metadata.get('failed_attempts', []):
    path = args.input/job['log_name']
    if hashlib.sha256(path.read_bytes()).hexdigest() != job['sha256']:
        parser.error('Changed failed-attempt log: '+job['name'])
rows = []
for job in metadata['jobs']:
    path = args.input/(job['name']+'.log')
    data = path.read_bytes()
    if job['exit'] or job['watchdog'] or hashlib.sha256(data).hexdigest() != job['sha256']:
        parser.error('Unqualified or changed log: '+job['name'])
    text = data.decode()
    current = [line for line in text.splitlines() if line.startswith('((workload .')]
    if len(current) != job['rows'] or any(mark in text for mark in ['*** ERROR', 'ERROR in']):
        parser.error('Invalid result rows: '+job['name'])
    rows.extend('(measurement '+json.dumps(job['name'])+' '+line+')' for line in current)
metadata['measurement_rows'] = len(rows)
args.output_stem.parent.mkdir(parents=True, exist_ok=True)
args.output_stem.with_suffix('.json').write_text(json.dumps(metadata, indent=2)+'\n')
header = '(receipt (schema . gerbil-parser.lexical-plan-matched.v1) (source-head . '+json.dumps(metadata['head'])+') (rows . '+str(len(rows))+') (qualification . '+metadata['qualification']+'))'
args.output_stem.with_suffix('.sexp').write_text(header+'\n'+'\n'.join(rows)+'\n')
print('SEALED', len(rows), metadata['head'], metadata['qualification'])
