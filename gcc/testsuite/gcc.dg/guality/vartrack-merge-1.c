/* { dg-do run } */
/* { dg-require-effective-target lto } */
/* { dg-options "-O2 -g -flto -fgnu89-inline -fno-ipa-value-specialization -fno-ipa-value-specialization-cold --param=max-jump-thread-duplication-stmts=20" } */

/* A core debug regression with the new specialization disabled.  The
   ordinary threader can form the extra predecessor with budget 20.  */
#include "guality.h"

inline int
read_b (int *p)
{
  return *p;
}

int
main (int argc, char **argv)
{
  int b = -1;
  GUALCHKVAL (b);
  if (argc > 0)
    b = -read_b (&b);
  GUALCHKVAL (b);
  return b;
}
