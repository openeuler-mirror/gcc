/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8-a+sve -maarch64-sve-memcall-inlining -maarch64-sve-memcall-runtime-check --param=aarch64-sve-memcall-size-threshold=256" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Constants at the threshold use SVE.  Constants above it use the normal
   memcpy or memset expansion.  Keep both sides together so that the test
   documents the boundary as one compiler decision.  */

/*
** copy_256:
**	...
**	cntb	x[0-9]+
**	...
**	whilelo	p[0-9]+\.b, xzr, x[0-9]+
**	...
**	ld1b	z[0-9]+\.b, p[0-9]+/z, \[x1, x[0-9]+\]
**	st1b	z[0-9]+\.b, p[0-9]+, \[x0, x[0-9]+\]
**	...
**	ret
*/
void
copy_256 (char *dst, const char *src)
{
  __builtin_memcpy (dst, src, 256);
}

/*
** copy_257:
**	mov	x2, 257
**	b	memcpy
*/
void
copy_257 (char *dst, const char *src)
{
  __builtin_memcpy (dst, src, 257);
}

/*
** set_256:
**	...
**	cntb	x[0-9]+
**	...
**	mov	z[0-9]+\.b, w1
**	whilelo	p[0-9]+\.b, xzr, x[0-9]+
**	...
**	st1b	z[0-9]+\.b, p[0-9]+, \[x0, x[0-9]+\]
**	...
**	ret
*/
void
set_256 (char *dst, int value)
{
  __builtin_memset (dst, value, 256);
}

/*
** set_zero_256:
**	...
**	cntb	x[0-9]+
**	...
**	mov	z[0-9]+\.b, #0
**	whilelo	p[0-9]+\.b, xzr, x[0-9]+
**	...
**	st1b	z[0-9]+\.b, p[0-9]+, \[x0, x[0-9]+\]
**	...
**	ret
*/
void
set_zero_256 (char *dst)
{
  __builtin_memset (dst, 0, 256);
}

/*
** set_257:
**	mov	x2, 257
**	b	memset
*/
void
set_257 (char *dst, int value)
{
  __builtin_memset (dst, value, 257);
}

