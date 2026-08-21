/* { dg-do compile } */
/* { dg-options "-O2 -mmul-widen128 -fdump-tree-mul_widen128-details -ffat-lto-objects" } */

/* The other operand position of the carry check.  A wrapping sum comes
   out below both of its addends, so `low < addc32' and `addc32 > low'
   are the same carry and both spellings are accepted - but the value in
   the addend slot has to be an addend of this addition, and that single
   test carries the whole rejection: the comparison is found on LO's use
   list, so whichever side LO is not on is the side being checked.
   mul-widen128-badcarry-1.c puts LO in the addend slot (`low > z');
   this test puts it in the sum slot (`z > low') so the unrelated value
   z lands in the addend slot instead.  Between them the two operand
   positions of the guard are both pinned - at one point the suite
   stayed green with part of this check removed while a 96/32 split
   compiled to a full 64x64 multiply.

   The low half is still a real low half and folds on its own, which is
   why the assertion names the full match.  */

typedef unsigned long long uint64_t;

void __attribute__((noipa))
bad_carry_addend (uint64_t in0, uint64_t in1, uint64_t z, uint64_t *hi,
		  uint64_t *lo)
{
  uint64_t al = in0 & 0xffffffffULL, ah = in0 >> 32;
  uint64_t bl = in1 & 0xffffffffULL, bh = in1 >> 32;
  uint64_t m10 = al * bh, m00 = al * bl, m01 = ah * bl, m11 = ah * bh;
  uint64_t addc = m10 + m01;
  m11 += (uint64_t) (addc < m01) << 32;
  m11 += addc >> 32;
  uint64_t addc32 = addc << 32;
  uint64_t low = m00 + addc32;
  m11 += (uint64_t) (z > low);	/* LO on the sum side, wrong addend */
  *lo = low;
  *hi = m11;
}

/* { dg-final { scan-tree-dump-not "rewrote schoolbook \\(" "mul_widen128" } } */
/* { dg-final { scan-assembler-not {\tumulh\t} } } */
