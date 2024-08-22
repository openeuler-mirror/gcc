/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=2 -fipa-pta -fllc-allocate -S -fdump-tree-llc_allocate-details-lineno" } */

int a, b;
int *d;
void f(void)
{
  int c;
  b %= 1;

  if(1 - (b < 1))
    {
      int *q = 0;

      if(a)
	{
	  c = 0;
lbl:
	  for(*d; *d; ++*d)
	    if(c ? : a ? : (c = 1) ? : 0)
	      *q &= 1;
	  return;
	}

      q = (int *)1;
    }
  goto lbl;
}

/* { dg-final { scan-tree-dump       "Unhandled scenario for non-ssa pointer." "llc_allocate" } } */
