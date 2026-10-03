#!/usr/bin/env python3
"""Pair every event-program workload and verify unchanged semantic/reuse counters."""
from pathlib import Path
import argparse, hashlib, json, math, re, statistics
p = argparse.ArgumentParser(description=__doc__)
p.add_argument('input', type=Path)
p.add_argument('output', type=Path)
a = p.parse_args()
metadata = json.loads((a.input/'metadata.json').read_text())
if not metadata.get('complete') or not metadata.get('event_program'):
    raise SystemExit('Require a complete event-program run')
pairs = {}
row_count = 0
for job in metadata['jobs']:
    data = (a.input/(job['name']+'.log')).read_bytes()
    if job['exit'] or job['watchdog'] or hashlib.sha256(data).hexdigest() != job['sha256']:
        raise SystemExit('Unqualified log: '+job['name'])
    variant = 'control' if job['name'].startswith('strict-') else 'candidate'
    repeat = int(job['name'].rsplit('-', 1)[1])
    lines = [line for line in data.decode().splitlines() if line.startswith('((workload .')]
    if len(lines) != job['rows']:
        raise SystemExit('Row count changed: '+job['name'])
    for line in lines:
        fields = dict(re.findall(r'\(([^ ()]+) \. ([^()]*)\)', line))
        key = tuple(fields.get(name, '') for name in ['workload', 'family', 'input-units', 'location', 'operation'])
        if (fields['samples'] != str(metadata['samples'])
            or fields['event-program-enabled?'] != ('#f' if variant == 'control' else '#t')
            or any(fields.get(name) != '#t' for name in ['complete-artifact-equal?', 'inverse-edit-equal?'])):
            raise SystemExit('Semantic/backend assertion changed: '+str(key))
        pair = pairs.setdefault((key, repeat), {})
        if variant in pair:
            raise SystemExit('Duplicate row: '+str(key))
        pair[variant] = fields
        row_count += 1
rows = {}
ignored = {'event-program-enabled?', 'fresh-cpu-median-ms', 'cached-cpu-median-ms'}
for (key, repeat), pair in pairs.items():
    if set(pair) != {'control', 'candidate'}:
        raise SystemExit('Missing pair: '+str(key))
    control, candidate = pair['control'], pair['candidate']
    invariant_keys = (set(control) | set(candidate))-ignored
    mismatch = [name for name in invariant_keys if control.get(name) != candidate.get(name)]
    before, after = float(control['cached-cpu-median-ms']), float(candidate['cached-cpu-median-ms'])
    if before <= 0 or after <= 0:
        raise SystemExit('Invalid median')
    rows.setdefault(key, {})[repeat] = dict(control_ms=before, candidate_ms=after,
                                           change_pct=100*(after/before-1),
                                           counter_differences={name: dict(control=control.get(name), candidate=candidate.get(name)) for name in sorted(mismatch)})
result = []
for key, orders in sorted(rows.items()):
    if set(orders) != {1, 2}:
        raise SystemExit('Missing execution order: '+str(key))
    changes = [orders[n]['change_pct'] for n in [1, 2]]
    trend = 'both-faster' if max(changes) < 0 else ('both-slower' if min(changes) > 0 else 'mixed')
    result.append(dict(key=key, control_ms=statistics.mean(orders[n]['control_ms'] for n in [1, 2]),
                       candidate_ms=statistics.mean(orders[n]['candidate_ms'] for n in [1, 2]),
                       changes_pct=changes, trend=trend, orders=orders))
if row_count != 112 or len(result) != 28:
    raise SystemExit('Require the entire 112-row / 28-workload matrix')
groups = {}
for row in result:
    groups.setdefault(' | '.join(row['key'][:2]), []).append(row)
summary = dict(schema='gerbil-parser.event-program-summary.v1', head=metadata['head'], complete=True,
               measurement_rows=row_count, paired_workloads=len(result),
               counters_equal=all(not order['counter_differences'] for row in result for order in row['orders'].values()), trends={trend: sum(row['trend'] == trend for row in result)
                                             for trend in ['both-faster', 'both-slower', 'mixed']},
               families={name: dict(workloads=len(group), geometric_change_pct_by_order=[
                   100*(math.exp(statistics.mean(math.log(row['orders'][n]['candidate_ms']/row['orders'][n]['control_ms'])
                                                for row in group))-1) for n in [1, 2]])
                         for name, group in groups.items()}, rows=result,
               method='Compare per-row CPU medians from 20 samples; preserve both execution orders and every scalar semantic/reuse counter',
               performance_admission=('rejected-counter-invariants' if any(order['counter_differences'] for row in result for order in row['orders'].values()) else ('rejected-request-cost' if all(row['trend'] == 'both-slower' for row in result) else 'Same-head experimental backend comparison; production default impact and full gates require separate qualification')))
a.output.parent.mkdir(parents=True, exist_ok=True)
a.output.write_text(json.dumps(summary, indent=2)+'\n')
print(json.dumps({name: summary[name] for name in ['head', 'measurement_rows', 'paired_workloads', 'counters_equal', 'trends', 'families']}, indent=2))
