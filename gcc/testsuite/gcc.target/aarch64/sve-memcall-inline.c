/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8-a+sve -maarch64-sve-memcall-inlining -maarch64-sve-memcall-runtime-check --param=aarch64-sve-memcall-size-threshold=256" } */

void do_it_set (char * out, int n, size_t size)
{
  __builtin_memset (out, n, size);
}

void copy1 (int *x, int *y, long z, int **res)
{
  __builtin_memcpy (x, y, z);
  *res = x;
}

/* { dg-final { scan-assembler "cntb\tx\[0-9\]"} } */
/* { dg-final { scan-assembler "whilelo\tp\[0-9\]+\.b"} } */
/* { dg-final { scan-assembler "ld1b\tz\[0-9\]+\.b" } } */
/* { dg-final { scan-assembler "st1b\tz\[0-9\]+\.b" } } */
/* { dg-final { scan-assembler "bl\tmemset"} } */
/* { dg-final { scan-assembler "bl\tmemcpy"} } */
