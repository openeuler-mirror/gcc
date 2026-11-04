/* { dg-do run } */
/* { dg-options "-O2 -ftree-constant-byte-range -fdump-tree-constbyterange-details" } */
/* { dg-final { scan-tree-dump "Constant byte object excludes search character" "constbyterange1" } } */
__attribute__((noipa)) int scan (unsigned n)
{
  const char *p = "abc|xyz|pq";
  while (n--)
    {
      const char *q = __builtin_strchr (p,'|');
      if (!q) break;
      p = q + 1;
    }
  if (*p == '%') __builtin_abort ();
  if (__builtin_strchr (p,'@')) __builtin_abort ();
  return *p;
}
int main (void)
{
  for (unsigned n=0;n!=20;++n)
    if (scan (n) != (n==0 ? 'a' : n==1 ? 'x' : 'p'))
      __builtin_abort ();
  return 0;
}
