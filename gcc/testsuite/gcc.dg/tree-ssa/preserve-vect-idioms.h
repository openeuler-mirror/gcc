/* A nested widened multiply reduction.  This header can be included more
   than once to check per-function options and different unroll counts.  */

#ifndef NAME
#define NAME f
#endif
#ifndef ATTR
#define ATTR noipa
#endif
#ifndef COUNT
#define COUNT 16
#endif

void __attribute__ ((ATTR))
NAME (int *__restrict output, const signed char *__restrict weights,
   const int *__restrict biases, const unsigned char *__restrict input)
{
  for (unsigned int i = 0; i < 32; ++i)
    {
      int sum = biases[i];
      for (unsigned int j = 0; j < COUNT; ++j)
	sum += weights[i * 32 + j] * input[j];
      output[i] = sum;
    }
}

#undef NAME
#undef ATTR
#undef COUNT
