/* { dg-do run } */
/* { dg-options "-Og -g" } */

#include "guality.h"

/* Derived from the lifetime shape in gcc.c-torture/execute/20070212-2.c.
   The batch test additionally observes immediately after the zero store,
   before an observation call can hide an incorrect old register location.  */
__attribute__((noipa)) int
vta_parameter_zero (int k, int i1, int j1)
{
  int *f1;
  if (k)
    f1 = &i1;
  else
    f1 = &j1;
  i1 = 0;
  /* Force evaluation before GDB assigns its convenience variables: a lazy
     unavailable value can otherwise leave guality's -1 result sentinel
     unchanged and be misreported as a wrong value.  Both computed results
     are nonzero (1 for zero, 2 for any nonzero value), avoiding the header's
     non-lvalue-zero ambiguity.  Its ++/-- probe cannot modify I1 through
     this non-lvalue expression; no generated-code I1 read is added.  */
  GUALCHKXPRVAL ("(i1 != 0) + 1", 1, 1);
  return *f1;
}

int
main (int argc, char **argv)
{
  if (vta_parameter_zero (0, 1, 2) != 2
      || vta_parameter_zero (1, 1, 2) != 0)
    abort ();
  return 0;
}
