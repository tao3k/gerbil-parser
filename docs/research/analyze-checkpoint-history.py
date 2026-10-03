#!/usr/bin/env python3
"""Verify qualified before/after operation counts for checkpoint history repair."""
from pathlib import Path
import argparse,json
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('before',type=Path)
p.add_argument('after',type=Path)
p.add_argument('output',type=Path)
a=p.parse_args()
old=json.loads(a.before.read_text());new=json.loads(a.after.read_text())
for data in [old,new]:
    if not (data['complete'] and data['owner_source_unchanged'] and data['owner_native_unchanged']
            and len(data['rows'])==26
            and all(row['complete-artifact-equal?']=='#t' for row in data['rows'])):
        raise SystemExit('Require complete isolated construction receipts')
for key in ['source_sha256','instrumented_sha256','harness_sha256']:
    if old[key]!=new[key]:
        raise SystemExit('Different construction instrumentation or workloads: '+key)
if not new.get('require_tail_resume'):
    raise SystemExit('Fixed receipt must require actual checkpoint resume')
previous={(row['input-units'],row['stage']):row for row in old['rows']}
current={(row['input-units'],row['stage']):row for row in new['rows']}
if set(previous)!=set(current) or len(previous)!=26:
    raise SystemExit('Construction stage coverage changed')
comparisons=[]
for key,row in current.items():
    if key[1] not in ['last-insert-inverse','last-delete-inverse']:
        continue
    before=previous[key]
    if not (before.get('freshFallback?')=='#t' and row.get('freshFallback?')!='#t'
            and int(row['checkpointReusedShiftCount'])>0):
        raise SystemExit('Fallback elimination was not demonstrated')
    counts={}
    for metric in ['append-calls','field-allocations','node-allocations','remainingSignificantTokenCount']:
        b,c=int(before[metric]),int(row[metric])
        if b<=0 or c>=b:
            raise SystemExit('No operation reduction: '+str(key)+' '+metric)
        counts[metric]=dict(before=b,after=c,reduction_pct=100*(1-c/b))
    comparisons.append(dict(input_units=int(key[0]),stage=key[1],
                            checkpoint_shifts=int(row['checkpointReusedShiftCount']),counts=counts))
result=dict(schema='gerbil-parser.checkpoint-history-comparison.v1',before_head=old['head'],after_head=new['head'],
            complete=len(comparisons)==4,
            scope='Instrumented operation counts; no CPU or complete-request speedup claim',
            comparisons=comparisons)
a.output.write_text(json.dumps(result,indent=2)+'\n')
print('CHECKPOINT-HISTORY-COMPARISON-OK',len(comparisons))
