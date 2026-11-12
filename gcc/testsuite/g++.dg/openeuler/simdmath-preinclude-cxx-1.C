// { dg-do compile }
// { dg-require-effective-target simdmath_header }
// { dg-options "-fsimdmath" }
// { dg-skip-if "needs a hosted TU with a standard include path" { *-*-* } { "-ffreestanding" "-nostdinc" } { "" } }

// The C++ half of the canary.  This suite has one other test file, so
// when the effective target above was derived from -fsimdmath's own
// behaviour a broken pre-include took the whole C++ side of the feature
// to zero tests and reported nothing.  Measured, by deleting the
// pre-include: 8 PASS became 0 PASS and 0 FAIL.

#ifndef __SIMDMATH_H__
#error -fsimdmath did not pre-include simdmath.h
#endif

int dummy;
