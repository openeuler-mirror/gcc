/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -march=armv8-a+sve -maarch64-sve-memcall-inlining -mno-aarch64-sve-memcall-runtime-check --param=aarch64-sve-memcall-size-threshold=256" } */

/* 513 bytes is larger than the maximum architectural SVE vector length, so
   it exercises multiple iterations when the run-time fallback is disabled.  */
#define SVE_MEMCALL_TEST_SIZES 0, 1, 256, 257, 513
#include "sve-memcall-inline-run.h"
