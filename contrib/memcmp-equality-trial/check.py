from pathlib import Path
import os, subprocess as sp, json, re, shlex, platform
import argparse
p=argparse.ArgumentParser(description='Standalone dynamic memcmp checks (native AArch64 GNU/Linux)')
p.add_argument('--cc',required=True,help='Compiler containing the memcmp options')
p.add_argument('--output',required=True,help='New directory for test outputs')
a=p.parse_args()
if platform.system() != 'Linux' or platform.machine().lower() not in ('aarch64', 'arm64'):
    p.error('this standalone harness requires native AArch64 GNU/Linux')
triple=sp.check_output([str(Path(a.cc).resolve()),'-dumpmachine'],text=True).strip()
if not triple.startswith('aarch64') or 'linux' not in triple:
    p.error('the compiler target must be AArch64 GNU/Linux')
HERE=Path(__file__).resolve().parent
T=Path(a.output).resolve(); T.mkdir(parents=True,exist_ok=False)
S=HERE.parents[1]/'gcc/testsuite/gcc.dg'; C=str(Path(a.cc).resolve())
rows=[]
def cmd(args,log):
    with log.open('w') as f: rc=sp.run(args,cwd=T,stdout=f,stderr=sp.STDOUT).returncode
    if rc: raise RuntimeError(f'{log}: exit {rc}')
def case(name,source,flags,run=True,scan=None):
    d=T/name; d.mkdir(exist_ok=False)
    out=d/('a.out' if run else 'out.s')
    args=[C,*flags,str(source),'-o',str(out)]
    if not run: args+=['-S']
    if run: args+=[str(T/'helpers.o')]
    cmd(args,d/'compile.log')
    if run: cmd([str(out)],d/'run.log')
    dumps='\n'.join(x.read_text() for x in d.glob('*expand*'))
    if scan:
        mode,text,count=scan
        actual=dumps.count(text)
        assert (actual==count if mode=='times' else actual==0 if mode=='not' else actual>0),(name,actual,scan)
    rows.append({'name':name,'command':args,'run':run,'passed':True})
    print('PASS',name,flush=True)
cmd([C,'-O3','-mcpu=native','-fno-lto','-fno-tree-loop-distribute-patterns','-c',str(HERE/'helpers.c'),'-o',str(T/'helpers.o')],T/'helpers-build.log')
for p in sorted([*S.glob('small-memcmp-equality-*.c'),*S.glob('memcmp-prefix-*.c')]):
    s=p.read_text(); flags=shlex.split(re.search(r'dg-options "([^"]*)"',s).group(1))
    m=re.search(r'scan-rtl-dump(-not|-times)? "([^"]*)"(?: (\d+))?',s)
    scan=None if not m else ('not' if m[1]=='-not' else 'times' if m[1]=='-times' else 'has',m[2],int(m[3] or 0))
    case('original-'+p.stem,p,flags,'dg-do run' in s,scan)
for k in [0,1,2,4,8,16]:
    for m in [0,1,2]:
        f=['-O2','-fchecking=2','-finline-memcmp-prefix','-finline-small-memcmp-equality',f'--param=memcmp-prefix-bytes={k}','--param=memcmp-prefix-skip-equal=1',f'--param=small-memcmp-equality-outline={m}']
        case(f'guard-k{k}-m{m}',HERE/'guard-ordered.c',f)
        case(f'semantics-k{k}-m{m}',S/'small-memcmp-equality-1.c',f)
        if k and m==0:
            case(f'suffix-k{k}',HERE/'suffix-test.c',f+[f'-DPREFIX={k}','-Wl,--wrap=memcmp'])
for m in [1,2]:
    name=f'lto-outline-{m}'
    case(name,S/'small-memcmp-equality-1.c',['-O3','-flto=auto','-flto-partition=one','-finline-small-memcmp-equality',f'--param=small-memcmp-equality-outline={m}'])
    asm=sp.check_output(['objdump','-d',str(T/name/'a.out')],text=True)
    symbol='__gcc_trial_memcmpeq_'+('bounded' if m==1 else 'loop')
    assert re.search(r'\b(?:b|bl)\s+[^\n]*<'+symbol+'>',asm),name
    (T/name/'disassembly.txt').write_text(asm)
for k in [1,2,4,8,16]:
    for m in [0,1,2]:
        case(f'combination-k{k}-m{m}',HERE/'combination-suffix-test.c',
             ['-O3','-flto','-flto-partition=one','-finline-memcmp-prefix',
              '-finline-small-memcmp-equality',f'--param=memcmp-prefix-bytes={k}',
              '--param=memcmp-prefix-skip-equal=1',f'--param=small-memcmp-equality-outline={m}',
              f'-DPREFIX={k}','-Wl,--wrap=memcmp','-Wl,--wrap=__gcc_trial_memcmpeq_bounded',
              '-Wl,--wrap=__gcc_trial_memcmpeq_loop'])
(T/'results.json').write_text(json.dumps({'completed':True,'passed':len(rows),'checks':rows},indent=2))
print('TOTAL',len(rows),'PASS',flush=True)
