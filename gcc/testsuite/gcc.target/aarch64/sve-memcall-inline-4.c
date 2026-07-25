/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8-a+sve -mstrict-align -maarch64-sve-memcall-inlining -maarch64-sve-memcall-runtime-check --param=aarch64-sve-memcall-size-threshold=256" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

typedef __SIZE_TYPE__ size_t;

/* Under strict alignment, byte-aligned operands use libc, whereas operands
   explicitly known to be 16-byte aligned still use SVE.  */

/*
** copy_u8:
**	...
**	bl	memcpy
**	...
**	ret
*/
void
copy_u8 (char *dst, const char *src, size_t size, char **result)
{
  __builtin_memcpy (dst, src, size);
  *result = dst;
}

/*
** set_u8:
**	...
**	bl	memset
**	...
**	ret
*/
void
set_u8 (char *dst, int value, size_t size, char **result)
{
  __builtin_memset (dst, value, size);
  *result = dst;
}

/*
** copy_a16:
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
copy_a16 (char *dst, const char *src, size_t size, char **result)
{
  char *aligned_dst = __builtin_assume_aligned (dst, 16);
  const char *aligned_src = __builtin_assume_aligned (src, 16);

  __builtin_memcpy (aligned_dst, aligned_src, size);
  *result = dst;
}

/*
** set_a16:
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
set_a16 (char *dst, int value, size_t size, char **result)
{
  char *aligned_dst = __builtin_assume_aligned (dst, 16);

  __builtin_memset (aligned_dst, value, size);
  *result = dst;
}

