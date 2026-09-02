/* { dg-do compile } */

static float
muladd (float x, float y, float z)
{
  return x * y + z;
}

float
g (float x[], long n)
{
  float r0 = 0, r1 = 0;
  for (; n; x += 2, n--)
    {
      r0 = muladd (x[0], x[0], r0);
      r1 = muladd (x[1], x[1], r1);
    }
  return r0 + r1;
}

/* { dg-final { scan-tree-dump "Choosing vector mode V8QI" "vect" } } */
