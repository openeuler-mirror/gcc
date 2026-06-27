/* { dg-do compile } */
/* { dg-options "-O3 -ftree-loop-invariant-split -fdump-tree-linvsplit-details --param loop-invariant-split-chunk-bytes=256" } */

float sink;

void
linvsplit_no_carry_store (const unsigned char *__restrict input,
			  const float *__restrict weights,
			  const float *__restrict bias)
{
  for (int i = 0; i < 64; ++i)
    {
      float acc = bias[i];
      for (int j = 0; j < 1024; ++j)
	acc += (float) input[j] * weights[i * 1024 + j];
      sink += acc;
    }
}

/* { dg-final { scan-tree-dump "loop invariant split: no carry store for inner live-out value" "linvsplit" } } */
/* { dg-final { scan-tree-dump-not "loop invariant split: split inner loop" "linvsplit" } } */

