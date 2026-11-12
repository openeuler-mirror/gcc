/* { dg-do run } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-O3 -ftree-loop-invariant-split --param loop-invariant-split-chunk-bytes=16 -fdump-tree-linvsplit-details" } */
/* A value computed in the outer loop body is live after the nest, and the
   outer trip count is a runtime value (guarded entry).  */
#define REF __attribute__ ((noipa, optimize ("O0")))
#define OPT __attribute__ ((noipa))
static unsigned int seed = 12345;

REF static int
rnd (void)
{
  seed = seed * 1103515245u + 12345u;
  return (seed >> 16) & 0x7f;
}

#define NJ 64
unsigned char in[NJ];
signed char w[64 * NJ];
int bias[64], out[64], ref[64];

OPT int
kernel (int n, int *__restrict out, const signed char *__restrict w,
	const int *__restrict bias, const unsigned char *__restrict in)
{
  int last = -1;
  for (int i = 0; i < n; ++i)
    {
      last = bias[i];
      int sum = last;
      for (int j = 0; j < NJ; ++j)
	sum += w[i * NJ + j] * in[j];
      out[i] = sum;
    }
  return last;
}

REF int
main (void)
{
  for (int j = 0; j < NJ; ++j)
    in[j] = rnd ();
  for (int x = 0; x < 64 * NJ; ++x)
    w[x] = rnd () - 64;
  for (int i = 0; i < 64; ++i)
    bias[i] = rnd () + 1;
  static const int ns[] = { 0, 1, 2, 5, 64 };
  for (unsigned t = 0; t < sizeof ns / sizeof ns[0]; ++t)
    {
      int n = ns[t];
      for (int i = 0; i < 64; ++i)
	out[i] = ref[i] = 7;
      int rlast = -1;
      for (int i = 0; i < n; ++i)
	{
	  rlast = bias[i];
	  int s = rlast;
	  for (int j = 0; j < NJ; ++j)
	    s += w[i * NJ + j] * in[j];
	  ref[i] = s;
	}
      if (kernel (n, out, w, bias, in) != rlast)
	__builtin_abort ();
      for (int i = 0; i < 64; ++i)
	if (out[i] != ref[i])
	  __builtin_abort ();
    }
  return 0;
}

/* { dg-final { scan-tree-dump-times "split inner loop into 4 chunks" 1 "linvsplit" } } */
