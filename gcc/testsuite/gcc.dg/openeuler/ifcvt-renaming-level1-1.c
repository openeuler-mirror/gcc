/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-fdump-rtl-ce1 -O2 --param max-rtl-if-conversion-unpredictable-cost=100 --param max-rtl-if-conversion-predictable-cost=100 --param=ifcvt-allow-register-renaming=1 -fifcvt-allow-complicated-cmps" } */

/* Level 1 renames registers inside the blocks being converted; level 2
   additionally rewrites the condition itself.  Measured on this tree: this
   source already reaches all seven conversions at level 1, so the two
   levels are pinned as equal here - a level-2-only case would need a
   condition whose registers the blocks overwrite. */


typedef unsigned int uint16_t;

uint16_t
foo (uint16_t x, uint16_t y, uint16_t z, uint16_t a,
     uint16_t b, uint16_t c, uint16_t d) {
  int i = 1;
  int j = 1;
  if (a > b) {
      j = x;
      if (b > c)
	i = y;
      else
	i = z;
    }
  else {
      j = y;
      if (c > d)
	i = z;
    }
  return i * j;
}


/* { dg-final { scan-rtl-dump "7 true changes made" "ce1" } } */
