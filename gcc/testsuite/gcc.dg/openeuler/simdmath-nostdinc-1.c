/* { dg-do compile } */
/* { dg-options "-fsimdmath -nostdinc -isystem ${srcdir}/gcc.dg/openeuler/nostdinc-inc" } */

/* -nostdinc must suppress the pre-include even where the header can
   still be found: the std_inc guard, not a failed header search, is
   what this pins.

   The header is therefore supplied deliberately, as a decoy that only
   defines the guard macro, on an -isystem path - which -nostdinc does
   not remove.  That makes the answer independent of the tree's state:
   an earlier version relied on whatever path the suite or the build
   tree happened to provide, and on a tree where the compiler had been
   installed, TEST_GCC_EXEC_PREFIX put the real header on the bracket
   chain that -nostdinc empties - so the pre-include found nothing with
   or without the guard and the test could not fail.  With the decoy,
   deleting the std_inc guard turns this red in every configuration.

   No dg-require simdmath_header: the decoy is self-contained, so the
   test also runs - and still discriminates - where the compiler ships
   no header at all.

   nohdr-simdmath-1.c is the other half - what the option does with
   nothing on the include path at all.  */

#ifdef __SIMDMATH_H__
#error -nostdinc did not suppress the -fsimdmath pre-include
#endif

int dummy;
