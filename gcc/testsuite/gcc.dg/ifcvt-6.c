/* { dg-do compile { target { aarch64*-*-* } } } */
/* { dg-options "-fdump-rtl-ce1 -O2 -fifcvt-allow-complicated-cmps --param max-rtl-if-conversion-unpredictable-cost=100 --param max-rtl-if-conversion-predictable-cost=100 --param=ifcvt-allow-register-renaming=2 " } */

typedef unsigned int uint16_t;

uint16_t
foo (uint16_t x, uint16_t y, uint16_t z, uint16_t a,
     uint16_t b, uint16_t c, uint16_t d)
{
  int i = 1;
  int j = 1;
  if (a > b)
    {
      j = x;
      if (b > c)
	i = y;
      else
	i = z;
    }
  else
    {
      j = y;
      if (c > d)
	i = z;
    }
  return i * j;
}

/* { dg-final { scan-rtl-dump "7 true changes made" "ce1" } } */
