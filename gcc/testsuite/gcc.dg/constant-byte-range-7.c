/* { dg-do compile } */
/* { dg-options "-O2 -ftree-constant-byte-range -fno-delete-null-pointer-checks -fdump-tree-constbyterange-details" } */
/* { dg-final { scan-tree-dump-not "Constant byte object excludes search character" "constbyterange1" } } */
/* { dg-final { scan-tree-dump-not "Constant byte object excludes search character" "constbyterange2" } } */
char *scan (unsigned n)
{
  const char *p = "abc|xyz|pq";
  while (n--)
    {
      const char *q = __builtin_strchr (p,'|');
      if (!q) break;
      p = q+1;
    }
  return __builtin_strchr (p,'@');
}
