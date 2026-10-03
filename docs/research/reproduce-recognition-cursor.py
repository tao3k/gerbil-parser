#!/usr/bin/env python3
"""Run the existing topology benchmark with two native candidate finders."""
from pathlib import Path
import subprocess,time,threading,signal,os,hashlib,sys,tempfile
current=str(Path.cwd())
source_head=subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip()
if subprocess.run(['git','diff','--quiet','7cb11943fa5cd4afcd6cf3f6abff8e1e14574f55','--','src','languages','t/benchmarks/incremental-session/benchmark.ss']).returncode:
    raise SystemExit('Runtime/language/benchmark source must match the measured head 7cb1194')
out=Path(sys.argv[1]) if len(sys.argv)>1 else Path(tempfile.mkdtemp(prefix='recognition-cursor-'))
out=out.resolve()
if any((parent/'gerbil.pkg').exists() for parent in [out,*out.parents]):
    raise SystemExit('Output directory must be outside a Gerbil package to preserve control module identity')
out.mkdir(parents=True,exist_ok=True)
baseline='catalog-control'
original=subprocess.check_output(['git','show','0032a6251ce423bed1b8894d5654bb29f1204c8c:src/runtime/reuse.ss'],text=True)
(out/'catalog-original.ss').write_text(original)
control=original.replace('../compiler/machine',':gerbil-parser/src/compiler/machine')
for module in ['lexer','probe','token','lr-parser']:
    control=control.replace('./'+module,':gerbil-parser/src/runtime/'+module)
assert '(stats (make-vector 9 0))' in control
control=control.replace('(stats (make-vector 9 0))','(stats (make-vector 10 0))')
control_path=out/'cursor-catalog-reuser.ss'
control_path.write_text(control)
print('COMPILE-CONTROL',control_path,flush=True)
subprocess.run(['gxc','-v','-O','-d',current+'/.gerbil/lib',str(control_path)],env=dict(os.environ,GERBIL_PATH=current+'/.gerbil'),check=True,timeout=180)
print('NATIVE-CONTROL-OK',flush=True)
# The receipt commit supplies this fixture. Binding replacement is process local.
progress=out/'progress.ss'
if not progress.exists():
    fixture=Path(current+'/t/fixtures/benchmark-progress.ss').read_text()
    progress.write_text(fixture)
harness=out/'benchmark.ss'
harness.write_bytes(Path(current+'/t/benchmarks/incremental-session/benchmark.ss').read_bytes())
print('SOURCE-HEAD',source_head,'OUTPUT',out,flush=True)
print('HARNESS-SHA256',hashlib.sha256(harness.read_bytes()).hexdigest(),flush=True)
flat=['topology-hcl-capture','400','800']; nested=['topology-hcl-nested-capture','2','8','16']
jobs=[('catalog-flat',baseline,flat,20),('cursor-no-probe-flat',current,['no-probe-reuse',*flat],20),('cursor-flat',current,flat,20),('catalog-flat-repeat',baseline,flat,20),('cursor-flat-repeat',current,flat,20),('catalog-nested',baseline,nested,20),('cursor-nested',current,nested,20),('catalog-nested-repeat',baseline,nested[:1]+['8','16'],20),('cursor-nested-repeat',current,nested[:1]+['8','16'],20),('cursor-flat-scale',current,['topology-hcl-capture','2','100','200','400','800'],5),('cursor-arithmetic',current,['topology-capture','2','400','800','1600','3200'],5)]
for name,cwd,args,samples in jobs:
    print('BENCHMARK',name,'SAMPLES',samples,'CWD',current,flush=True)
    catalog=cwd==baseline
    cwd=current
    activity=[time.monotonic()];failed=threading.Event()
    env=dict(os.environ,GERBIL_PATH=cwd+'/.gerbil',GERBIL_PARSER_BENCHMARK_SAMPLES=str(samples),GERBIL_PARSER_LR_TRACE='1')
    proc=subprocess.Popen(['gxi','-:max-heap=1G,debug=q',str(progress),str(harness),*(['catalog-control'] if catalog else []),*args],cwd=cwd,env=env,stdin=subprocess.DEVNULL,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,text=True,start_new_session=True)
    def guard():
        started=time.monotonic()
        while proc.poll() is None:
            if time.monotonic()-activity[0]>5 or time.monotonic()-started>180:
                failed.set()
                try:os.killpg(proc.pid,signal.SIGKILL)
                except ProcessLookupError:pass
                return
            time.sleep(.1)
    monitor=threading.Thread(target=guard,daemon=True);monitor.start()
    rows=0;bad=False
    with (out/(name+'.log')).open('w') as f:
        for line in proc.stdout:
            activity[0]=time.monotonic();f.write(line);f.flush()
            if line.startswith('((workload . significant-token-topology-change)'):rows+=1
            if '*** ERROR' in line or 'ERROR in' in line:bad=True
            print(line,end='',flush=True)
    status=proc.wait();monitor.join()
    if failed.is_set() or status or bad or rows!=6*(len(args)-1-(1 if args[0]=='no-probe-reuse' else 0)):
        raise SystemExit('FAILED '+name+' '+str((status,rows,failed.is_set(),bad)))
    print('BENCHMARK-OK',name,'ROWS',rows,flush=True)
print('MATCHED-ALL-OK',flush=True)
