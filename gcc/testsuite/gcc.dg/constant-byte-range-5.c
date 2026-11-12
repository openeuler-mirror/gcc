/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-constant-byte-range -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump "__builtin_strchr" "optimized" } } */
char *absent (unsigned n)
{
  const char *p = "abcxyz";
  while (n-- && *p) ++p;
  return __builtin_strchr (p, '@');
}
