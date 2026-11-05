/* { dg-do compile } */
/* { dg-options "-O2 -std=gnu17 -fexcess-precision=16 -ffp-model=strict" } */

/* A value-safe model must not take away a value-safety request.
   -fexcess-precision=16 asks for a rounding step after every _Float16
   operation; appending -ffp-model=strict used to discard it outright,
   because the undo was written against frontend_set_flag_excess_precision,
   which no front end sets and which a command-line option does not set
   either - so the test was always true.  Measured then: fcvt went from
   six to four and __FLT_EVAL_METHOD__ from 16 to 0, and only in this
   order, since writing the model first left the option alone.

   The model still undoes an excess-precision setting that came from
   -ffast-math rather than from the user; that one never reaches
   opts_set.

   -std=gnu17 by name.  A -std= board option would otherwise reach this
   through the front end rather than through the model - measured, an
   -std=c99 board zeroes __FLT_EVAL_METHOD__ on its own - and the #error
   below would then accuse an option that had nothing to do with it.  */

#if __FLT_EVAL_METHOD__ != 16
#error -ffp-model=strict discarded -fexcess-precision=16
#endif

_Float16
f (_Float16 a, _Float16 b, _Float16 c)
{
  return a * b + c;
}
