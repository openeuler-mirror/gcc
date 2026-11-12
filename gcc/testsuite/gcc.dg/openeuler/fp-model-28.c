/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O2 -fcx-limited-range -ffp-model=strict" } */

/* -fcx-limited-range is one of the pieces -ffast-math turns on, so a
   value-safe model has to take it back off.  set_fast_math_flags cannot:
   it sets this one under `if (set)', so passing 0 leaves it standing.
   The model undoes it separately.

   Without that, -ffp-model=strict left complex division unranged and
   __GCC_IEC_559_COMPLEX at 0, in a model whose whole promise is
   value-safe arithmetic.  -fno-fast-math has the same hole upstream; it
   is left alone there, since changing it would move every target.  */

_Complex double
f (_Complex double a, _Complex double b)
{
  return a / b;
}

/* Ranged division is a libcall; the limited-range form is inlined.  */
/* { dg-final { scan-assembler "__divdc3" } } */
