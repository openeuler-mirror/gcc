/* { dg-do compile } */
/* { dg-options "-O3 -floop-unroll-and-jam -fno-tree-loop-im -flim-after-jam --param unroll-jam-min-percent=0 -fdump-tree-unrolljam-details" } */
/* { dg-require-effective-target int32plus } */

extern unsigned int a[32][1024];

void __attribute__((noinline,noclone))
foo (unsigned int n, unsigned int m, unsigned int x)
{
  unsigned int i, j;

  for (i = 1; i < m; ++i)
    for (j = 1; j < n; ++j)
      {
	unsigned int t = (i + x) * (i + x + 3);
	a[i][j] = a[i + 1][j] + t;
      }
}

/* { dg-final { scan-tree-dump "applying unroll and jam" "unrolljam" } } */
/* { dg-final { scan-tree-dump "Moving statement" "unrolljam" } } */
