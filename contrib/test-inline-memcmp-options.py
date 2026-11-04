#!/usr/bin/env python3
"""Check option-family defaults, alias precedence, and code equivalence.

Usage: python3 contrib/test-inline-memcmp-options.py GCC [BASELINE_GCC]
Uses temporary files and requires objcopy. BASELINE_GCC is optional and should
be the compiler before the naming-only change. No performance claims are made.
"""
import json
import os
from pathlib import Path
import re
import shlex
import subprocess as sp
import sys
import tempfile

GCC = Path(sys.argv[1]).resolve()
BASELINE = Path(sys.argv[2]).resolve() if len(sys.argv) > 2 else None
ROOT = Path(__file__).resolve().parents[1]
ALIASES = {
    'finline-small-memcmp-equality': 'finline-memcmp-small-equality',
}
PARAMS = {}
GROUPS = {
    'finline-memcmp': ['finline-memcmp-small-equality', 'finline-memcmp-prefix'],
}
ENV = os.environ.copy()
ENV.pop('LD_PRELOAD', None)
COUNT = 0

def driver(gcc):
    return [str(gcc)] + (['-B' + str(gcc.parent) + '/'] if gcc.name == 'xgcc' else [])

def run(args, cwd=None):
    r = sp.run(args, cwd=cwd, env=ENV, capture_output=True, text=True, timeout=180)
    if r.returncode:
        raise AssertionError(f'{shlex.join(args)}\n{r.stdout}\n{r.stderr}')
    return r.stdout

def check(ok, name):
    global COUNT
    if not ok:
        raise AssertionError(name)
    COUNT += 1

def states(flags, category='optimizers'):
    out = run(driver(GCC) + flags + ['-Q', '--help=' + category,
                                    '-x', 'c', '-c', '/dev/null', '-o', '/dev/null'])
    return {re.sub(r'<[^>]*>$', '', p[0]): p[1] for line in out.splitlines() if len(p := line.split()) == 2}

def neg(name):
    return '-fno-' + name[1:]

for level in ['-O0', '-O2', '-O3']:
    for group, children in GROUPS.items():
        for flags, expected in [([], False), (['-' + group], True),
                                (['-' + group, neg(group)], False)]:
            st = states([level] + flags)
            for child in children:
                check(st.get('-' + child) == ('[enabled]' if expected else '[disabled]'),
                      str((level, flags, child, st.get('-' + child))))
        for child in children:
            old = next((k for k, v in ALIASES.items() if v == child), child)
            cases = [([neg(child), '-' + group], False),
                     (['-' + group, neg(child)], False),
                     ([neg(old), '-' + group], False),
                     (['-' + group, neg(old)], False),
                     (['-' + group, neg(old), '-' + child], True),
                     (['-' + group, '-' + old, neg(child)], False),
                     (['-' + group, neg(child), '-' + old], True),
                     (['-' + group, '-' + child, neg(old)], False)]
            for flags, expected in cases:
                st = states([level] + flags)
                check(st.get('-' + child) == ('[enabled]' if expected else '[disabled]'),
                      str((level, flags, child, st.get('-' + child))))
    for flags in [['-finline-small-memcmp-equality'], ['-finline-memcmp-small-equality']]:
        st = states([level] + flags)
        check(st.get('-finline-memcmp-small-equality') == '[enabled]' and
              st.get('-finline-memcmp-prefix') == '[disabled]', 'M1 remains independent')
    st = states([level, '-finline-memcmp-prefix'])
    check(st.get('-finline-memcmp-small-equality') == '[disabled]' and
          st.get('-finline-memcmp-prefix') == '[enabled]', 'M2 remains independent')

st = states([], 'params')
for old, (new, default) in PARAMS.items():
    check(st.get('--param=' + new + '=') == str(default), 'default ' + new)
    for flags, expected in [(['--param=' + old + '=17'], 17),
                            (['--param=' + new + '=19'], 19),
                            (['--param=' + old + '=17', '--param=' + new + '=19'], 19),
                            (['--param=' + new + '=19', '--param=' + old + '=17'], 17)]:
        actual = states(flags, 'params').get('--param=' + new + '=')
        check(actual == str(expected), str((flags, actual)))
for key, value in [('memcmp-prefix-bytes', 4), ('memcmp-prefix-skip-equal', 1)]:
    check(st.get('--param=' + key + '=') == str(value), 'default ' + key)

def legacy(flags):
    result = []
    for flag in flags:
        for old, new in ALIASES.items():
            flag = flag.replace(new, old).replace('fno-' + new[1:], 'fno-' + old[1:])
        for old, (new, _) in PARAMS.items():
            flag = flag.replace(new, old)
        if flag == '-finline-memcmp':
            result.extend('-' + c for c in ['finline-small-memcmp-equality', 'finline-memcmp-prefix']
                          if not any(f in ['-' + c, neg(c), '-' + ALIASES.get(c, c),
                                           neg(ALIASES.get(c, c))] for f in flags))
        else:
            result.append(flag)
    return result

fixtures = [
    'gcc.dg/memcmp-family-default.c',
    'gcc.dg/memcmp-family-alias-off.c',
    'gcc.dg/memcmp-family-alias-before.c',
]
with tempfile.TemporaryDirectory(prefix='gcc-option-families-') as td:
    work = Path(td)
    for rel in fixtures:
        source = ROOT / 'gcc/testsuite' / rel
        text = source.read_text()
        flags = shlex.split(re.search(r'dg-options "([^"]*)"', text)[1])
        flags = [f for f in flags if not f.startswith('-fdump-')]
        executable = 'dg-do run' in text
        for lto in [False, True] if executable else [False]:
            opts = flags + (['-flto', '-flto-partition=one'] if lto else [])
            blobs = []
            variants = [(GCC, opts), (GCC, legacy(opts))]
            if BASELINE:
                variants.append((BASELINE, legacy(opts)))
            for i, (gcc, options) in enumerate(variants):
                output = work / 'out'
                run(driver(gcc) + options + ['-frandom-seed=0', str(source)] +
                    ([] if executable else ['-c']) + ['-o', str(output)], work)
                if executable:
                    run([str(output)], work)
                binary = work / 'text.bin'
                run(['objcopy', '-O', 'binary', '--only-section=.text', str(output), str(binary)], work)
                blobs.append(binary.read_bytes())
            check(all(b == blobs[0] for b in blobs), 'code equivalence ' + rel + str(lto))
            print('PASS code equivalence', rel, 'LTO' if lto else 'normal', flush=True)
print(json.dumps({'passed': COUNT, 'failures': 0, 'baseline_compared': bool(BASELINE)}))