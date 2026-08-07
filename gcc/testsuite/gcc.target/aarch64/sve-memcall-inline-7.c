/* { dg-do compile } */
/* { dg-options "-O2 -march=armv9-a+sme -maarch64-sve-memcall-inlining -mno-aarch64-sve-memcall-runtime-check" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

typedef __SIZE_TYPE__ size_t;

/* Streaming mode makes TARGET_SIMD false while TARGET_SVE remains true.
   Armv9-A does not enable MOPS, so this exercises SVE setmem expansion when
   neither SIMD nor MOPS is available.  */

/*
** set_memcall:
**	...
**	whilelo	p[0-9]+\.b, xzr, x[0-9]+
**	mov	z[0-9]+\.b, w1
**	...
**	st1b	z[0-9]+\.b, p[0-9]+, \[x[0-9]+, x[0-9]+\]
**	...
**	ret
*/
void
set_memcall (char *dst, int value, size_t size) __arm_streaming
{
  __builtin_memset (dst, value, size);
}

/* { dg-final { scan-assembler-not "bl\tmemset" } } */
