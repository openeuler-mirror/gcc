/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8-a -ftree-loop-invariant-split -fdump-tree-linvsplit-details -fdump-tree-vect-details --param loop-invariant-split-max-copies=32" } */

void
linvsplit_neon (const float *__restrict input,
		const float *__restrict weights,
		const float *__restrict bias,
		float *__restrict output)
{
  for (int i = 0; i < 64; ++i)
    {
      float acc = bias[i];
      for (int j = 0; j < 1024; ++j)
	acc += input[j] * weights[i * 1024 + j];
      output[i] = acc;
    }
}

/* { dg-final { scan-tree-dump "loop invariant split target: vector_bytes=16, vector_regs=32, budget=256" "linvsplit" } } */
/* { dg-final { scan-tree-dump "loop invariant split: split inner loop" "linvsplit" } } */
/* { dg-final { scan-tree-dump-times "optimized: loop vectorized" 16 "vect" } } */
