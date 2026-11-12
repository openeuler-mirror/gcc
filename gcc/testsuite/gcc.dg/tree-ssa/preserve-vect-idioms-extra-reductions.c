/* { dg-do compile } */
/* { dg-options "-O3 -fpreserve-vect-idioms -fdump-tree-cunrolli-details" } */

/* Additional simple integer reductions must not hide the multiply sum.  */

void __attribute__ ((noipa))
add (int *__restrict output, int *__restrict reductions,
      const signed char *__restrict weights, const int *__restrict biases,
      const unsigned char *__restrict input)
{
  for (unsigned int i = 0; i < 32; ++i)
    {
      int sum = biases[i];
      int extra = i;
      for (unsigned int j = 0; j < 16; ++j)
	{
	  int w = weights[i * 32 + j];
	  sum += w * input[j];
	  extra += (unsigned char) w;
	}
      output[i] = sum;
      reductions[i] = extra;
    }
}

void __attribute__ ((noipa))
minimum (int *__restrict output, int *__restrict reductions,
      const signed char *__restrict weights, const int *__restrict biases,
      const unsigned char *__restrict input)
{
  for (unsigned int i = 0; i < 32; ++i)
    {
      int sum = biases[i];
      int extra = 127;
      for (unsigned int j = 0; j < 16; ++j)
	{
	  int w = weights[i * 32 + j];
	  sum += w * input[j];
	  extra = extra < w ? extra : w;
	}
      output[i] = sum;
      reductions[i] = extra;
    }
}

void __attribute__ ((noipa))
maximum (int *__restrict output, int *__restrict reductions,
      const signed char *__restrict weights, const int *__restrict biases,
      const unsigned char *__restrict input)
{
  for (unsigned int i = 0; i < 32; ++i)
    {
      int sum = biases[i];
      int extra = -128;
      for (unsigned int j = 0; j < 16; ++j)
	{
	  int w = weights[i * 32 + j];
	  sum += w * input[j];
	  extra = extra > w ? extra : w;
	}
      output[i] = sum;
      reductions[i] = extra;
    }
}

/* { dg-final { scan-tree-dump-times "preserving widened multiply reduction" 3 "cunrolli" } } */
/* { dg-final { scan-tree-dump-not "completely unrolled" "cunrolli" } } */
