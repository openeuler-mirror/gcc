/* { dg-do compile } */
/* { dg-require-effective-target simdmath_vectorizes } */
/* { dg-options "-O3 -fsimdmath" } */

/* A translation unit that defines one of the eleven keeps the full
   upstream clone set.  That is what node->definition is for, and it had
   no test: the covered-function check alone cannot distinguish this from
   a bare declaration, and this is the case that matters - a vector math
   library is built by compiling exactly these definitions, and it must
   not lose its own 64-bit variants to an option meant for its callers.  */

#pragma omp declare simd notinbranch
float sinf (float x);

float
sinf (float x)
{
  return x * x + 1.0f;
}

void
loop (double *p, double *q, float *r, float *s, int n)
{
  int i;

  for (i = 0; i < n; i++)
    {
      p[i] = q[i] + 1.0;
      r[i] = sinf (s[i]);
    }
}

/* { dg-final { scan-assembler "_ZGVnN2v_sinf" } } */
