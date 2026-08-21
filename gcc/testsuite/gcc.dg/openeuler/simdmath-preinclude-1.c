/* { dg-do compile } */
/* { dg-require-effective-target simdmath_header } */
/* { dg-options "-fsimdmath" } */
/* { dg-skip-if "needs a hosted TU with a standard include path" { *-*-* } { "-ffreestanding" "-nostdinc" } { "" } } */

/* Pin the pre-include itself, on the one symptom that nothing else can
   produce.  Every other trace of it - a vector call in the assembly, a
   redeclaration diagnostic - also depends on vectorization, on the
   optimization level, or on what else the translation unit declares, so a
   test built on one of those goes quiet rather than red when the
   pre-include stops happening.

   The effective target above asks whether the compiler ships the header,
   by including it explicitly and without -fsimdmath.  That is a different
   question from the one being tested here, which is the whole point: an
   effective target derived from -fsimdmath's own behaviour would answer
   "no header" exactly when this test should fail.  */

#ifndef __SIMDMATH_H__
#error -fsimdmath did not pre-include simdmath.h
#endif

int dummy;
