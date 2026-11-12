/* { dg-do compile } */
/* { dg-options "-O3 -ftree-loop-invariant-split -fdump-tree-linvsplit-details --param loop-invariant-split-chunk-bytes=256" } */

void
linvsplit_alias_unknown (const unsigned char *input,
			 const float *weights,
			 const float *bias,
			 float *output)
{
  for (int i = 0; i < 64; ++i)
    {
      float acc = bias[i];
      for (int j = 0; j < 1024; ++j)
	acc += (float) input[j] * weights[i * 1024 + j];
      output[i] = acc;
    }
}

/* { dg-final { scan-tree-dump "loop invariant split: alias or dependence analysis is unknown" "linvsplit" } } */
/* { dg-final { scan-tree-dump-not "loop invariant split: split inner loop" "linvsplit" } } */
