/* { dg-do run } */
/* { dg-options "-Og -g -fno-ipa-value-specialization -fno-ipa-value-specialization-cold --param=max-jump-thread-duplication-stmts=20" } */

/* Exercise both entering values at a call.  Neither constant is the
   guality checker's unavailable-value sentinel, and B has no use after
   its final check.  The specialization feature is disabled.  */
#include "guality.h"

__attribute__((noipa)) void
check_merge (int n)
{
  int b = -3;
  GUALCHKVAL (b);
  if (n > 0)
    b = 5;
  GUALCHKVAL (b);
  /* Retain the caller frame without retaining B.  */
  __asm__ volatile ("" ::: "memory");
}

int
main (int argc, char **argv)
{
  static const int inputs[] = { -__INT_MAX__ - 1, -3, 0, 1, 2, 3,
                                __INT_MAX__ };
  for (unsigned i = 0; i < sizeof inputs / sizeof inputs[0]; ++i)
    check_merge (inputs[i]);
  return 0;
}
