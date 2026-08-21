/* -fchrec-mul-fold-strict-overflow is a retired openEuler gcc-12
   option: the PR114074 strict-overflow CHREC folding it gated is
   built into GCC 14 (the upstream tests pr68317.c and
   torture/pr114074.c pass on this tree, and chrec_fold_multiply
   performs the gated operand canonicalization unconditionally).
   The positive form must be rejected with the explanation.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fchrec-mul-fold-strict-overflow" } */

int
f (int x)
{
  return x + 1;
}

/* { dg-error "is not needed on GCC 14" "" { target *-*-* } 0 } */
