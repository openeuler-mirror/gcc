/* { dg-do compile } */
/* { dg-options "-O3 -ftree-loop-invariant-split -fdump-tree-linvsplit-details --param loop-invariant-split-chunk-bytes=256" } */

extern "C" void
linvsplit_affine (const unsigned char *__restrict input,
		  const float *__restrict weights,
		  const float *__restrict bias,
		  float *__restrict output)
{
  for (int i = 0; i < 64; ++i)
    {
      float acc = bias[i];
      for (int j = 0; j < 1024; ++j)
	acc += (float) input[j] * weights[i * 1024 + j];
      output[i] = acc;
    }
}

/* { dg-final { scan-tree-dump "loop invariant split: split inner loop into 4 chunks" "linvsplit" } } */
/* { dg-final { scan-tree-dump "chunk 0: .0, 256." "linvsplit" } } */
/* { dg-final { scan-tree-dump "chunk 1: .256, 512." "linvsplit" } } */
/* { dg-final { scan-tree-dump "chunk 2: .512, 768." "linvsplit" } } */
/* { dg-final { scan-tree-dump "chunk 3: .768, 1024." "linvsplit" } } */
