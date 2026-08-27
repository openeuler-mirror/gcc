/* { dg-do compile } */
/* { dg-options "-fsimdmath -nostdinc" } */

/* -fsimdmath is meant to sit in a project's CFLAGS, so it reaches
   translation units built without the standard include path and builds
   where the header was never installed at all (--disable-libgomp,
   --enable-languages=c).  Missing declarations mean no vectorization,
   not a failed build; the Fortran half has always skipped a missing
   simdmath_f.h silently, and the C half used to die with

     <command-line>: fatal error: simdmath.h: No such file or directory

   Named nohdr-* so that openeuler.exp compiles it before any include
   path is added.  Since the guard gained std_inc this passes either way,
   measured - -nostdinc now skips the pre-include whatever is on the path
   - but the ordering is kept as insurance for that guard, this being the
   only place the option is exercised with an empty search path.  */

int
main (void)
{
  return 0;
}
