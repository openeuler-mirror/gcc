# Dynamic memcmp equality and prefix experiments

The standalone harness requires native AArch64 GNU/Linux.  Its native CPU
flags, guard pages, linker wrapping and disassembly assertions are not a
portable cross-target validation suite.

Both compiler switches are opt-in; neither is enabled by optimization levels.
Use these options at compilation and at the LTO link:

```
-O3 -finline-small-memcmp-equality -finline-memcmp-prefix
```

This selects inline bounded equality (M1) and a four-byte ordered prefix
with proven-equal bytes skipped in the fallback (M2). Defaults are
`small-memcmp-equality-outline=0`, `memcmp-prefix-bytes=4`, and
`memcmp-prefix-skip-equal=1`.

To specify these defaults explicitly:

```
-O3 -finline-small-memcmp-equality -finline-memcmp-prefix \
  --param=memcmp-prefix-bytes=4 --param=memcmp-prefix-skip-equal=1
```

The prefix length accepts 0 through 16; 0 disables prefix expansion. Each
load is dominated by a length check. Suffix adjustment occurs only after
proving that the input is longer than the prefix and all prefix bytes match.
On equality calls, the suffix may then enter M1. Larger prefixes increase
code size; performance is workload-dependent. To restore the original one-byte
full-fallback strategy, set `--param=memcmp-prefix-bytes=1` and
`--param=memcmp-prefix-skip-equal=0`.

## Optional external equality functions

`small-memcmp-equality-outline=1` uses the bounded helper; 2 uses a byte loop.
They replace only calls eligible for M1, not general ordered comparisons.
These diagnostic modes require explicitly linking this helper object:

```sh
gcc -O3 -fno-lto -fno-tree-loop-distribute-patterns \
  -c contrib/memcmp-equality-trial/helpers.c -o memcmp-helpers.o
g++ -O3 -flto -finline-small-memcmp-equality \
  --param=small-memcmp-equality-outline=1 program.cc memcmp-helpers.o -o program
```

The helper is deliberately non-LTO and `noipa`. Its symbols are experimental,
not an installed libgcc ABI. Modes 0 (including the four-byte configuration
above) need no helper object. Do not enable modes 1 or 2 without providing it.

## Validation

The GCC testsuite contains the original compile/runtime and negative-gate
checks. This standalone GNU/Linux harness additionally checks all sampled
prefixes, guard pages, mismatch positions, equality strategies, real helper
calls under LTO, and the exact fallback pointer/length arguments:

```sh
python3 contrib/memcmp-equality-trial/check.py \
  --cc /path/to/built/gcc --output /path/to/new-test-directory
```

Use a compatible runtime library search path if required by the compiler.
The harness needs GNU binutils (`objdump`, linker `--wrap`) and mmap/mprotect;
the validation configuration used AArch64 GNU/Linux. Results are written only
after all 71 checks and their assembly assertions succeed. This does not
replace a full GCC DejaGNU regression run or cross-target validation.
