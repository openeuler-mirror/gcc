/* { dg-do compile } */
/* { dg-options "-O2 -mmul-widen128 -fdump-tree-mul_widen128-details -ffat-lto-objects" } */

/* The operands have to be 64 bits wide.  This is the same schoolbook
   spelled in __int128, where the halves are 96 and 32 bits rather than
   32 and 32 - so the result is not x * y, and the missing xh * yh term
   cannot be recovered by a widening multiply.  With the width check
   removed this folded to a full 128x128 multiply and was wrong on every
   input tried.  */

typedef unsigned __int128 uint128_t;

uint128_t __attribute__((noipa))
bad_width (uint128_t x, uint128_t y)
{
  uint128_t xl = x & 0xffffffffULL, xh = x >> 32;
  uint128_t yl = y & 0xffffffffULL, yh = y >> 32;

  return xl * yl + ((xl * yh + xh * yl) << 32);
}

/* { dg-final { scan-tree-dump-not "rewrote schoolbook" "mul_widen128" } } */
