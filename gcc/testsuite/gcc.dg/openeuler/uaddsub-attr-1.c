/* -fuaddsub-overflow-match-all is per-function optimization state (the
   option record carries the Optimization keyword): the optimize
   attribute can enable it in a translation unit compiled without the
   flag.  Before the keyword was added the attribute was silently
   ineffective.

   The shape must be one the flag actually gates: on aarch64 the narrow
   8+8->16 form has no direct overflow instruction, so upstream refuses
   it without the flag (the wide 64+64->128 form is matched even without
   it and would make this test vacuous).  The TU is compiled WITHOUT the
   flag: the attributed function must be matched, the plain copy must
   not.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#include <stdint.h>

__attribute__ ((optimize ("uaddsub-overflow-match-all"))) uint16_t
add16_attr (uint8_t a, uint8_t b)
{
  uint8_t tmp = a + b;
  uint8_t overflow = tmp < a;
  return ((uint16_t) overflow << 8) + tmp;
}

uint16_t
add16_plain (uint8_t a, uint8_t b)
{
  uint8_t tmp = a + b;
  uint8_t overflow = tmp < a;
  return ((uint16_t) overflow << 8) + tmp;
}

/* { dg-final { scan-tree-dump-times "ADD_OVERFLOW" 1 "optimized" } } */
