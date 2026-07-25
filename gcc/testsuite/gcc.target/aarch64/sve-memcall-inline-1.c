/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8-a+sve -maarch64-sve-memcall-inlining -maarch64-sve-memcall-runtime-check --param=aarch64-sve-memcall-size-threshold=256" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

typedef __SIZE_TYPE__ size_t;

/* Variable-sized operations use an SVE loop up to the run-time threshold and
   fall back to a libcall for larger sizes.  */

/*
** set_variable:
**	cmp	x2, 256
**	bhi	.*
**	...
**	cntb	x[0-9]+
**	...
**	whilelo	p[0-9]+\.b, xzr, x[0-9]+
**	...
**	st1b	z[0-9]+\.b, p[0-9]+, \[x[0-9]+, x[0-9]+\]
**	...
**	bl	memset
**	...
*/
void
set_variable (char *dst, int value, size_t size)
{
  __builtin_memset (dst, value, size);
}

/*
** copy_variable:
**	...
**	cmp	x2, 256
**	bhi	.*
**	...
**	cntb	x[0-9]+
**	...
**	whilelo	p[0-9]+\.b, xzr, x[0-9]+
**	...
**	ld1b	z[0-9]+\.b, p[0-9]+/z, \[x[0-9]+, x[0-9]+\]
**	st1b	z[0-9]+\.b, p[0-9]+, \[x[0-9]+, x[0-9]+\]
**	...
**	bl	memcpy
**	...
*/
void
copy_variable (char *dst, const char *src, size_t size, char **result)
{
  __builtin_memcpy (dst, src, size);
  *result = dst;
}
