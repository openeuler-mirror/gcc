/* The negative form is the de-facto state and must stay silently
   accepted (see alias-expand-ssa-err-1.c).  */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-alias-analysis-expand-ssa" } */

int
f (int x)
{
  return x + 1;
}
