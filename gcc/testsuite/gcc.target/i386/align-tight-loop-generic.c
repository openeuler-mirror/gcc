/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-options "-O2 -g0 -fcf-protection=branch -mtune=generic" } */
/* { dg-final { scan-assembler-not "\\.p2align 5" } } */

char *
foo (char *dest, const char *src)
{
  while ((*dest++ = *src++) != '\0')
    /* nothing */;
  return --dest;
}
