/* { dg-do compile } */
/* { dg-options "-O3 --param loop-invariant-split-chunk-bytes=256 -fdump-tree-linvsplit-details" } */
/* Enabling the pass for one function only.  */
__attribute__ ((optimize ("tree-loop-invariant-split"))) void
kernel (int *__restrict out, const signed char *__restrict w,
	const int *__restrict bias, const unsigned char *__restrict in)
{
  for (int i = 0; i < 64; ++i)
    {
      int sum = bias[i];
      for (int j = 0; j < 1024; ++j)
	sum += w[i * 1024 + j] * in[j];
      out[i] = sum;
    }
}

/* { dg-final { scan-tree-dump-times "optimized: loop invariant split: split inner loop into 4 chunks" 1 "linvsplit" } } */
