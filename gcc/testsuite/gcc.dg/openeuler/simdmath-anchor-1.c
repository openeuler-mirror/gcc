/* { dg-do compile } */
/* { dg-options "-specs=${srcdir}/gcc.dg/openeuler/simdmath-anchor.spec" } */

/* The removed spellings are kept as anchors that the driver diagnoses by
   name.  Each also needs a case in common_handle_option: the arm that
   catches unhandled options asserts that they have a flag variable, and
   an anchor deliberately has none, so leaving them out turns any route
   that reaches cc1 into

     internal compiler error: in common_handle_option

   rather than a clean rejection.  The driver normally errors first,
   which is why this went unnoticed and why an ordinary dg-options
   cannot reach it - the option never gets past the driver.  A specs
   file appends to the cc1 command line after driver option handling is
   over, so it does.

   The spec carries every anchor spelling this compiler has - including
   the negative form, which reaches the same anchor with value 0 - so
   that dropping any one of the cases brings the ICE back here.  An
   earlier version covered only -fftz and would have stayed green with
   any other case removed.  */

int
main (void)
{
  return 0;
}
