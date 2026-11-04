/* { dg-do run } */
/* { dg-options "-O2 -ftree-static-byte-cfg -fchecking=2" } */

#define DEFINE_SCAN(NAME, ATTR) \
__attribute__((noipa)) ATTR int NAME (unsigned key) \
{ \
  const unsigned char *p = (const unsigned char *) \
    "abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"; \
  int result = 0; \
  while (*p) \
    { \
      if (*p == key) result += 3; \
      else if (*p == ' ') result -= 1; \
      else result += *p & 7; \
      ++p; \
    } \
  return result; \
}

DEFINE_SCAN (specialized, )
DEFINE_SCAN (reference, __attribute__((optimize("no-tree-static-byte-cfg"))))

int
main (void)
{
  for (unsigned key = 0; key < 300; ++key)
    if (specialized (key) != reference (key))
      __builtin_abort ();
  return 0;
}
