/* { dg-do run } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-O3 -ftree-loop-invariant-split --param loop-invariant-split-chunk-bytes=256 -fdump-tree-linvsplit-details" } */
/* GEMV-style y[i] += A[i][j] * x[j]: the accumulator is initialized from
   the carry address itself.  */
#define REF __attribute__ ((noipa, optimize ("O0")))
#define OPT __attribute__ ((noipa))
static unsigned int seed = 12345;

REF static int
rnd (void)
{
  seed = seed * 1103515245u + 12345u;
  return (seed >> 16) & 0x7f;
}

#define NI 64
#define NJ 1024
unsigned char in[NJ];
signed char w[NI * NJ];
int out[NI], ref[NI];

OPT void
kernel (int *__restrict out, const signed char *__restrict w,
	const unsigned char *__restrict in)
{
  for (int i = 0; i < NI; ++i)
    {
      int sum = out[i];
      for (int j = 0; j < NJ; ++j)
	sum += w[i * NJ + j] * in[j];
      out[i] = sum;
    }
}

REF int
main (void)
{
  for (int j = 0; j < NJ; ++j)
    in[j] = rnd ();
  for (int x = 0; x < NI * NJ; ++x)
    w[x] = rnd () - 64;
  for (int i = 0; i < NI; ++i)
    out[i] = ref[i] = rnd ();
  for (int i = 0; i < NI; ++i)
    {
      int s = ref[i];
      for (int j = 0; j < NJ; ++j)
	s += w[i * NJ + j] * in[j];
      ref[i] = s;
    }
  kernel (out, w, in);
  for (int i = 0; i < NI; ++i)
    if (out[i] != ref[i])
      __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "split inner loop into 4 chunks" 1 "linvsplit" } } */
