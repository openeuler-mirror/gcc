/* { dg-do compile } */
/* { dg-options "-O2 -ftree-static-byte-cfg -fdump-tree-static-byte-cfg-details" } */
extern int hit (int) __attribute__ ((pure, nothrow));
extern int miss (int) __attribute__ ((pure, nothrow));

int
__attribute__((cold)) dbg1 (const unsigned char *s)
{
  const unsigned char *p = (const unsigned char *) "abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc";
  int r = 0;
  while (*p)
    {
      unsigned char c = *s++;
      if (c == *p)
        r += hit (r) ^ hit (r + 1) ^ hit (r + 2) ^ hit (r + 3);
      else
        r -= miss (r) ^ miss (r + 1) ^ miss (r + 2) ^ miss (r + 3);
      unsigned char seen = c;   /* 之后只剩一条 debug bind */
      (void) seen;
      ++p;
    }
  return r;
}

extern int hit (int) __attribute__ ((pure, nothrow));
extern int miss (int) __attribute__ ((pure, nothrow));

int
hot_control (const unsigned char *s)
{
  const unsigned char *p = (const unsigned char *) "abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc";
  int r = 0;
  while (*p)
    {
      unsigned char c = *s++;
      if (c == *p)
        r += hit (r) ^ hit (r + 1) ^ hit (r + 2) ^ hit (r + 3);
      else
        r -= miss (r) ^ miss (r + 1) ^ miss (r + 2) ^ miss (r + 3);
      unsigned char seen = c;   /* 之后只剩一条 debug bind */
      (void) seen;
      ++p;
    }
  return r;
}
/* { dg-final { scan-tree-dump "RESIDUAL-EMIT hot_control" "static-byte-cfg" } } */
/* { dg-final { scan-tree-dump-not "RESIDUAL-EMIT dbg1" "static-byte-cfg" } } */
