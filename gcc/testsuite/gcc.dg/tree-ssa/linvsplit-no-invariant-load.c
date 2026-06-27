/* { dg-do compile } */
/* { dg-options "-O3 -ftree-loop-invariant-split -fdump-tree-linvsplit-details --param loop-invariant-split-chunk-bytes=256" } */

void
linvsplit_no_invariant_load (const float *__restrict weights,
			     const float *__restrict bias,
			     float *__restrict output)
{
  for (int i = 0; i < 64; ++i)
    {
      float acc = bias[i];
      for (int j = 0; j < 1024; ++j)
	acc += weights[i * 1024 + j];
      output[i] = acc;
    }
}

/* { dg-final { scan-tree-dump "loop invariant split: no outer-invariant inner load" "linvsplit" } } */
/* { dg-final { scan-tree-dump-not "loop invariant split: split inner loop" "linvsplit" } } */

