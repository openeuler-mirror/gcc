/* { dg-do compile } */
/* { dg-options "-Ofast -Ofast -ffp-model=precise" } */

/* The degradation report is issued once for the whole command line, not
   once per -Ofast.  A project that gets -Ofast from two places - a
   makefile and a spec file, say - must not be told twice.

   dg-regexp rather than dg-warning: dg-warning is satisfied by any
   number of copies of the message, which is exactly what this needs to
   rule out.  dg-regexp prunes one occurrence, and a second lands in
   excess errors.  */

int
main (void)
{
  return 0;
}

/* { dg-regexp {.*warning: '-Ofast' is degraded to '-O3' due to '-ffp-model=precise'} } */
