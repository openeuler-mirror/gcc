/* { dg-do compile { target x86_64-*-* } } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O3 -fnon-temporal-store -march=x86-64 -mtune=generic --param=l2-cache-size=512 -fdump-tree-vect-details" } */

/* The two output streams cannot both be aligned by peeling.  Their
   nontemporal candidates must not prevent ordinary vectorization.  */
#define N 4000000
double a[N + 8], b[N + 8], c[N + 8];

void
f (void)
{
  for (long i = 0; i < N; i++)
    {
      a[i + 1] = c[i] * 2.0;
      b[i] = c[i] + 1.0;
    }
}

/* { dg-final { scan-tree-dump "Retrying alignment without nontemporal store requirements" "vect" } } */
/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
