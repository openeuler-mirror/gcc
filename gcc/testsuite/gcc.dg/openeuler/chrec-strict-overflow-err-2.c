/* The negative form is the de-facto state and must stay silently
   accepted (see chrec-strict-overflow-err-1.c).  */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-chrec-mul-fold-strict-overflow" } */

int
f (int x)
{
  return x + 1;
}
