/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O2 -fcx-limited-range" } */

/* The control for fp-model-28.c.  Without a model in the way
   -fcx-limited-range does what it says, so that test cannot be passing
   because complex division is a libcall no matter what.  */

_Complex double
f (_Complex double a, _Complex double b)
{
  return a / b;
}

/* { dg-final { scan-assembler-not "__divdc3" } } */
