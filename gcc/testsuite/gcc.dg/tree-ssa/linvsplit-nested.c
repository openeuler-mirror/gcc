/* { dg-do run } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-O3 -ftree-loop-invariant-split --param loop-invariant-split-chunk-bytes=64 -fdump-tree-linvsplit-details" } */
/* The split nest is itself nested in an outer K loop.  */
#define REF __attribute__ ((noipa, optimize ("O0")))
#define OPT __attribute__ ((noipa))
static unsigned int seed = 12345;

REF static int
rnd (void)
{
  seed = seed * 1103515245u + 12345u;
  return (seed >> 16) & 0x7f;
}

#define NK 3
#define NI 16
#define NJ 256
unsigned char in[NK * NJ];
signed char w[NK * NI * NJ];
int bias[NK * NI], out[NK * NI], ref[NK * NI];

OPT void
kernel (int *__restrict out, const signed char *__restrict w,
	const int *__restrict bias, const unsigned char *__restrict in)
{
  for (int k = 0; k < NK; ++k)
    for (int i = 0; i < NI; ++i)
      {
	int sum = bias[k * NI + i];
	for (int j = 0; j < NJ; ++j)
	  sum += w[(k * NI + i) * NJ + j] * in[k * NJ + j];
	out[k * NI + i] = sum;
      }
}

REF int
main (void)
{
  for (int j = 0; j < NK * NJ; ++j)
    in[j] = rnd ();
  for (int x = 0; x < NK * NI * NJ; ++x)
    w[x] = rnd () - 64;
  for (int x = 0; x < NK * NI; ++x)
    bias[x] = rnd ();
  for (int k = 0; k < NK; ++k)
    for (int i = 0; i < NI; ++i)
      {
	int s = bias[k * NI + i];
	for (int j = 0; j < NJ; ++j)
	  s += w[(k * NI + i) * NJ + j] * in[k * NJ + j];
	ref[k * NI + i] = s;
      }
  kernel (out, w, bias, in);
  for (int x = 0; x < NK * NI; ++x)
    if (out[x] != ref[x])
      __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "split inner loop into 4 chunks" 1 "linvsplit" } } */
