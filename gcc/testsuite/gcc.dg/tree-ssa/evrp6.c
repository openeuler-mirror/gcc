/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp1-details" } */

extern void abort (void);

int
foo (int k, int j)
{
  if (j >= 10)
    {
      if (j < k)
	{
	  k++;
	  if (k < 10)
	    abort ();
	}
    }

  return j;
}
/* { dg-final { scan-tree-dump "\\\[12, \\+INF" "evrp1" } } */
