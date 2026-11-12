/* { dg-do compile } */
/* { dg-options "-O2 -ftree-constant-byte-range -fdump-tree-constbyterange-details -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump "Constant byte object range" "constbyterange1" } } */
/* { dg-final { scan-tree-dump "Constant byte object excludes search character" "constbyterange1" } } */
/* { dg-final { scan-tree-dump-not "forbidden" "optimized" } } */
extern void forbidden (void);
int scan (unsigned n)
{
  const char *p = "abcxyz";
  int sum = 0;
  while (n-- && *p)
    {
      if (*p == '%') forbidden ();
      sum += *p++;
    }
  return sum;
}
char *absent (unsigned n)
{
  const char *p = "abcxyz";
  while (n-- && *p) ++p;
  return __builtin_strchr (p, '@');
}
