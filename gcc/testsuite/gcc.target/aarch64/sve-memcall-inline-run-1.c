/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -march=armv8-a+sve -maarch64-sve-memcall-inlining -maarch64-sve-memcall-runtime-check --param=aarch64-sve-memcall-size-threshold=256" } */

/* Exercise the empty, partial-vector and threshold boundary cases.  The last
   size must take the run-time libcall fallback.  */
#define SVE_MEMCALL_TEST_SIZES 0, 1, 255, 256, 257
#include "sve-memcall-inline-run.h"
