/* { dg-do compile } */
/* { dg-options "-O2 -mmul-widen128 -fdump-tree-mul_widen128-details" } */

/* The bignum formulation of the schoolbook with both carries written as
   `if' statements - how OpenSSL BN and friends actually spell it.

   Not currently matched: the carries reach the pass as PHI diamonds,
   and the matchers only accept the flattened comparison spelling.  An
   earlier revision flattened `c ? x + 2^k : x' with a match.pd pattern,
   which made this shape match but perturbed PRE and SLSR on unrelated
   code and turned three upstream tree-ssa tests red; the pattern was
   removed, and the plan of record is to recognize the un-flattened
   diamond inside the pass instead.  See the first item of the TODO
   above find_quadrant_mul in aarch64-mul-widen128.cc.

   The xfail assertions below record the gap rather than hiding it: they
   turn XPASS - which this project's acceptance bar treats as red - on
   the day the diamond carry is recognized, so the gap cannot be closed
   silently.  Today the shape gets nothing at all, not even the
   low-half-only fallback: the carry diamonds split the low half's own
   addition chain across blocks, and match_add_lo_parts requires it in
   one.  The non-xfail assertion pins that, so a partial recognition
   cannot appear or disappear unnoticed either.  */

typedef unsigned long long uint64_t;

void __attribute__((noipa))
bn_mul64 (uint64_t in0, uint64_t in1, uint64_t *hi, uint64_t *lo)
{
  uint64_t al = in0 & 0xffffffffULL, ah = in0 >> 32;
  uint64_t bl = in1 & 0xffffffffULL, bh = in1 >> 32;
  uint64_t m10 = al * bh, m00 = al * bl, m01 = ah * bl, m11 = ah * bh;
  uint64_t addc = m10 + m01;
  if (addc < m01)
    m11 += (uint64_t) 1 << 32;
  m11 += addc >> 32;
  uint64_t addc32 = addc << 32;
  uint64_t low = m00 + addc32;
  if (low < addc32)
    m11++;
  *lo = low;
  *hi = m11;
}

/* The xfail assertion names the full match: a bare "rewrote schoolbook"
   would be satisfied by the low-half-only fallback and could never turn
   XPASS.  */
/* { dg-final { scan-tree-dump-times "rewrote schoolbook \\(" 1 "mul_widen128" { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tumulh\t} 1 { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-not "rewrote schoolbook low half" "mul_widen128" } } */
