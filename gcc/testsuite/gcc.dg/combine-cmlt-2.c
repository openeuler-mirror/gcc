/* { dg-do compile { target aarch64-*-* } } */
/* { dg-options "-O3 -mcmlt-arith -mcpu=hip12" } */

/* The test checks usage of cmlt insns for arithmetic/logic calculations
 * in foo ().  It's inspired by sources of x264 codec.  */

typedef unsigned short int uint16_t;
typedef unsigned int uint32_t;

void foo( uint32_t *a, uint32_t *b)
{
  for (unsigned i = 0; i < 4; i++)
    {
      uint32_t s = ((a[i]>>((8 * sizeof(uint16_t))-1))
		    &(((uint32_t)1<<(8 * sizeof(uint16_t)))+1))*((uint16_t)-1);
      b[i] = (a[i]+s)^s;
    }
}

/* { dg-final { scan-assembler-times {cmlt\t} 1 } }  */
