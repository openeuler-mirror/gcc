/* { dg-do run } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-O3 -ftree-loop-invariant-split --param loop-invariant-split-chunk-bytes=256 -fdump-tree-linvsplit-details" } */
/* The carry address is read for a purpose other than initializing the
   accumulator.  */
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
int bias[NI], out[NI], ref[NI];

OPT void
kernel (int *__restrict out, const signed char *__restrict w,
	const int *__restrict bias, const unsigned char *__restrict in)
{
  for (int i = 0; i < NI; ++i)
    {
      int sum = bias[i];
      for (int j = 0; j < NJ; ++j)
	sum += w[i * NJ + j] * in[j] + (out[i] & 3);
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
    bias[i] = rnd ();
  for (int i = 0; i < NI; ++i)
    out[i] = ref[i] = rnd ();
  for (int i = 0; i < NI; ++i)
    {
      int s = bias[i], c = ref[i] & 3;
      for (int j = 0; j < NJ; ++j)
	s += w[i * NJ + j] * in[j] + c;
      ref[i] = s;
    }
  kernel (out, w, bias, in);
  for (int i = 0; i < NI; ++i)
    if (out[i] != ref[i])
      __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-not "split inner loop into" "linvsplit" } } */
/* { dg-final { scan-tree-dump "loop-carried memory dependence" "linvsplit" } } */
