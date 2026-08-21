/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O2 -fcx-limited-range -ffp-model=precise" } */

/* fp-model-28.c pins this for strict; undo_residual_fast_math_flags is
   called from all three value-safe models and only that one had a test,
   so deleting the call from either of the other two left the suite
   green.  Ranged division is a libcall; the limited-range form is
   inlined.  */

_Complex double
f (_Complex double a, _Complex double b)
{
  return a / b;
}

/* { dg-final { scan-assembler "__divdc3" } } */
