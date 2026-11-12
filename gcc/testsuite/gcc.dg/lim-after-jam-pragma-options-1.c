/* { dg-do compile } */
/* { dg-options "-O3 -floop-unroll-and-jam -fno-tree-loop-im --param unroll-jam-min-percent=0 -fdump-tree-unrolljam-details -flim-after-jam" } */
/* { dg-require-effective-target int32plus } */

/* A pragma can disable the additional LIM while keeping jam.  */
#pragma GCC push_options
#pragma GCC optimize ("no-lim-after-jam")
extern unsigned int a[32][1024];

void __attribute__ ((noinline, noclone))
foo (unsigned int n, unsigned int m, unsigned int x)
{
  for (unsigned int i = 1; i < m; ++i)
    for (unsigned int j = 1; j < n; ++j)
      {
	unsigned int t = (i + x) * (i + x + 3);
	a[i][j] = a[i + 1][j] + t;
      }
}

#pragma GCC pop_options

/* { dg-final { scan-tree-dump "applying unroll and jam" "unrolljam" } } */
/* { dg-final { scan-tree-dump-not "Moving statement" "unrolljam" } } */
