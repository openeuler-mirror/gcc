/* { dg-do compile} */
/* { dg-options "-O2 -fdump-tree-evrp1" } */


unsigned char
foo (unsigned int x)
{
  _Bool y = x;
  return (((unsigned char) ~0) >> y) * 2;
}

unsigned char
bar (unsigned int x)
{
  return (((unsigned char) ~0) >> (_Bool) x) * 2;
}

unsigned
baz (unsigned int x)
{
  if (x >= 4) return 32;
  return (-1U >> x) * 16;
}

/* { dg-final { scan-tree-dump-times  "254" 2 "evrp1" } }  */
/* { dg-final { scan-tree-dump "= PHI <32.*, 4294967280" "evrp1" } }  */

