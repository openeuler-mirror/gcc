/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize --param=vect-alias-flexible-segment-len=1" } */

#define TYPE int
#define SIZE 257

void __attribute__ ((weak))
f (TYPE *x, TYPE *y, unsigned short n, long m __attribute__((unused)))
{
  for (int i = 0; i < SIZE; ++i)
    x[i * n] += y[i * n];
}

/* { dg-final { scan-assembler {\tld1w\tz[0-9]+} } } */
/* { dg-final { scan-assembler {\tst1w\tz[0-9]+} } } */
/* { dg-final { scan-assembler {\tldr\tw[0-9]+} } } */
/* { dg-final { scan-assembler {\tstr\tw[0-9]+} } } */
/* Should use a WAR check that multiplies by (VF-2)*4 rather than
   an overlap check that multiplies by (257-1)*4.  */
/* { dg-final { scan-assembler {\tcntb\t(x[0-9]+)\n.*\tsub\tx[0-9]+, \1, #8\n.*\tmul\tx[0-9]+,[^\n]*\1} } } */
/* One range check and a check for n being zero.  */
/* { dg-final { scan-assembler-times {\t(?:cmp|tst)\t} 2 } } */
/* { dg-final { scan-assembler-times {\tccmp\t} 1 } } */
