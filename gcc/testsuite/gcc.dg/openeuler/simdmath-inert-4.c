/* { dg-do compile } */
/* { dg-options "-O2 -fsimdmath -ffp-model=fast" } */

/* The other side of the pair: fast and normal keep every function
   vectorizable, so there is nothing to report and the compile must stay
   quiet.  Without this the three tests above would pass just as well
   against a compiler that warned unconditionally.  */

int
main (void)
{
  return 0;
}
