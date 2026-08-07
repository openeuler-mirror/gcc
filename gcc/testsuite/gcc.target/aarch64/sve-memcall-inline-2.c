/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8-a+sve -maarch64-sve-memcall-inlining -mno-aarch64-sve-memcall-runtime-check --param=aarch64-sve-memcall-size-threshold=256" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

typedef __SIZE_TYPE__ size_t;

/* Without a run-time check, variable-sized operations use an SVE loop and
   have no libcall fallback.  */

/*
** copy_memcall:
**	...
**	cntb	x[0-9]+
**	whilelo	p[0-9]+\.b, xzr, x[0-9]+
**	...
**	ld1b	z[0-9]+\.b, p[0-9]+/z, \[x[0-9]+, x[0-9]+\]
**	st1b	z[0-9]+\.b, p[0-9]+, \[x[0-9]+, x[0-9]+\]
**	...
**	ret
*/

/*
** set_memcall:
**	...
**	cntb	x[0-9]+
**	...
**	mov	z[0-9]+\.b, w1
**	whilelo	p[0-9]+\.b, xzr, x[0-9]+
**	...
**	st1b	z[0-9]+\.b, p[0-9]+, \[x[0-9]+, x[0-9]+\]
**	...
**	ret
*/

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

/* { dg-final { scan-assembler-not "bl\tmemcpy" } } */
/* { dg-final { scan-assembler-not "bl\tmemset" } } */
