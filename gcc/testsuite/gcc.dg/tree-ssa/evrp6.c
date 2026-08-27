/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp1-details -fdump-tree-mergephi1" } */

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
/* { dg-final { scan-tree-dump "\\\[11, \\+INF" "evrp1" } } */
/* { dg-final { scan-tree-dump-not "abort" "mergephi1" } } */
