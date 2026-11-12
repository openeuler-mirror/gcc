/* { dg-do compile } */
/* { dg-options "-O2 -ftree-constant-byte-range -fwrapv-pointer -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump "__builtin_strchr" "optimized" } } */
char *absent (unsigned n)
{
  const char *p = "abcxyz";
  while (n-- && *p) ++p;
  return __builtin_strchr (p, '@');
}
