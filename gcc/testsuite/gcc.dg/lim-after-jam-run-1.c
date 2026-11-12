/* { dg-do run } */
/* { dg-options "-O3 -floop-unroll-and-jam -fno-tree-loop-im -flim-after-jam --param unroll-jam-min-percent=0 -fdump-tree-unrolljam-details" } */
/* { dg-require-effective-target int32plus } */

unsigned int a[32][1024], expected[32][1024];

void __attribute__ ((noipa))
candidate (unsigned int n, unsigned int m, unsigned int x)
{
  for (unsigned int i = 1; i < m; ++i)
    for (unsigned int j = 1; j < n; ++j)
      {
	unsigned int t = (i + x) * (i + x + 3);
	a[i][j] = a[i + 1][j] + t;
      }
}

/* Keep the reference and test driver out of the loop optimizer.  */
void __attribute__ ((noipa, optimize ("O0")))
reference (unsigned int n, unsigned int m, unsigned int x)
{
  for (unsigned int i = 1; i < m; ++i)
    for (unsigned int j = 1; j < n; ++j)
      expected[i][j] = expected[i + 1][j] + (i + x) * (i + x + 3);
}

int __attribute__ ((optimize ("O0")))
main (void)
{
  /* Include empty and single-iteration loops, odd and even trip counts,
     array boundaries and defined unsigned overflow in the invariants.  */
  const unsigned int ns[] = { 0, 1, 2, 3, 17, 1023, 1024 };
  const unsigned int ms[] = { 0, 1, 2, 3, 7, 30, 31 };
  const unsigned int xs[] = { 0, 1, ~0u, ~3u, 123456789u };

  for (unsigned int ni = 0; ni < sizeof ns / sizeof ns[0]; ++ni)
    for (unsigned int mi = 0; mi < sizeof ms / sizeof ms[0]; ++mi)
      for (unsigned int xi = 0; xi < sizeof xs / sizeof xs[0]; ++xi)
	{
	  for (unsigned int i = 0; i < 32; ++i)
	    for (unsigned int j = 0; j < 1024; ++j)
	      a[i][j] = expected[i][j]
		= (i * 1024 + j) * 2654435761u + xs[xi];

	  candidate (ns[ni], ms[mi], xs[xi]);
	  reference (ns[ni], ms[mi], xs[xi]);

	  for (unsigned int i = 0; i < 32; ++i)
	    for (unsigned int j = 0; j < 1024; ++j)
	      if (a[i][j] != expected[i][j])
		__builtin_abort ();
	}

  return 0;
}

/* { dg-final { scan-tree-dump "applying unroll and jam" "unrolljam" } } */
/* { dg-final { scan-tree-dump "Moving statement" "unrolljam" } } */
