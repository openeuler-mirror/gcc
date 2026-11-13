/* { dg-do compile { target x86_64-*-* } } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O3 -fnon-temporal-store -march=x86-64 -mtune=generic --param=l2-cache-size=512 -fdump-tree-slp-details" } */

/* The recurrence prevents loop vectorization, but BB SLP can combine the
   adjacent stores.  NT candidates must not block ordinary unaligned stores,
   whether their misalignment is known or their alignment is unknown.  */
#define N 4000000
double a[2 * N + 8], c[2 * N + 8];
double t;

void
known_misalignment (void)
{
  double s = t;
  for (long i = 0; i < N; i++)
    {
      c[2 * i + 1] = a[2 * i] * 2.0 + s;
      c[2 * i + 2] = a[2 * i + 1] * 3.0 + s;
      s = s * 0.5 + a[2 * i];
    }
  t = s;
}

void
unknown_alignment (double *__restrict dst, const double *__restrict src)
{
  double s = t;
  for (long i = 0; i < N; i++)
    {
      dst[2 * i] = src[2 * i] * 2.0 + s;
      dst[2 * i + 1] = src[2 * i + 1] * 3.0 + s;
      s = s * 0.5 + src[2 * i];
    }
  t = s;
}

/* { dg-final { scan-tree-dump-times "basic block part vectorized" 2 "slp1" } } */
