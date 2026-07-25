/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8-a -maarch64-sve-memcall-inlining" } */

/* Enabling the feature alone is insufficient: TARGET_SVE must also hold.  */

typedef __SIZE_TYPE__ size_t;

void
copy_memcall (char *dst, const char *src, size_t size, char **result)
{
  __builtin_memcpy (dst, src, size);
  *result = dst;
}

void
set_memcall (char *dst, int value, size_t size, char **result)
{
  __builtin_memset (dst, value, size);
  *result = dst;
}

/* { dg-final { scan-assembler-not "cntb\tx\[0-9\]+" } } */
/* { dg-final { scan-assembler-not "whilelo\tp\[0-9\]+\\.b" } } */
/* { dg-final { scan-assembler-times "bl\tmemcpy" 1 } } */
/* { dg-final { scan-assembler-times "bl\tmemset" 1 } } */
