/* { dg-do run } */
/* { dg-options "-ftree-loop-invariant-split --param loop-invariant-split-chunk-bytes=16 --param loop-invariant-split-max-copies=16" } */

__attribute__((noipa,optimize("O0")))
void
linvsplit_ref (const unsigned char *__restrict input,
	       const float *__restrict weights,
	       const float *__restrict bias,
	       float *__restrict output)
{
  for (int i = 0; i < 8; ++i)
    {
      float acc = bias[i];
      for (int j = 0; j < 64; ++j)
	acc += (float) input[j] * weights[i * 64 + j];
      output[i] = acc;
    }
}

void
linvsplit_opt (const unsigned char *__restrict input,
	       const float *__restrict weights,
	       const float *__restrict bias,
	       float *__restrict output)
{
  for (int i = 0; i < 8; ++i)
    {
      float acc = bias[i];
      for (int j = 0; j < 64; ++j)
	acc += (float) input[j] * weights[i * 64 + j];
      output[i] = acc;
    }
}

static unsigned char input[64];
static float weights[8 * 64];
static float bias[8];
static float ref_output[8];
static float opt_output[8];

static float
absf (float x)
{
  return x < 0 ? -x : x;
}

int
main (void)
{
  for (int t = 0; t < 4; ++t)
    {
      for (int j = 0; j < 64; ++j)
	input[j] = (unsigned char) ((j * 17 + t * 11) & 31);
      for (int i = 0; i < 8; ++i)
	bias[i] = (float) (i * 3 - t) * 0.25f;
      for (int i = 0; i < 8 * 64; ++i)
	weights[i] = (float) (((i + 5 * t) * 7) % 23 - 11) * 0.125f;

      linvsplit_ref (input, weights, bias, ref_output);
      linvsplit_opt (input, weights, bias, opt_output);

      for (int i = 0; i < 8; ++i)
	if (absf (ref_output[i] - opt_output[i]) > 0.001f)
	  __builtin_abort ();
    }

  return 0;
}

