/* -falias-analysis-expand-ssa is a retired openEuler gcc-12 option
   whose gated logic is built into GCC 14 (the PR106019 scenario;
   upstream test bb-slp-pr106019.c passes on this tree).  The positive
   form must be rejected with the explanation, not silently accepted
   (it controlled nothing here) and not 'unrecognized option'.  */
/* { dg-do compile } */
/* { dg-options "-O2 -falias-analysis-expand-ssa" } */

int
f (int x)
{
  return x + 1;
}

/* { dg-error "is not needed on GCC 14" "" { target *-*-* } 0 } */
