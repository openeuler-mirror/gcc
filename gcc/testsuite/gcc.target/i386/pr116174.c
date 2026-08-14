/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-options "-O2 -g0 -fcf-protection=branch -mtune=c86-4g-m7" } */
/* { dg-final { scan-assembler "endbr64\\n\\t\\.p2align 5" } } */
char *
foo (char *dest, const char *src)
{
  while ((*dest++ = *src++) != '\0')
    /* nothing */;
  return --dest;
}
