/* { dg-do compile } */
/* { dg-options "-O2 -ftree-fold-phiopt -mbmi" } */

int foo (int x, int y)
{
  if (~x & y)
    return 1;

  return 0;
}

int bar (int x, int y)
{
  if ((~x & y) > 0)
    return 1;

  return 0;
}

/* { dg-final { scan-assembler-not "test" } } */
