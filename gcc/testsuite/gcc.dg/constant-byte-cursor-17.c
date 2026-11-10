/* { dg-do run } */
/* { dg-options "-O3 -fdelete-null-pointer-checks -ftree-constant-byte-range-cursor -fdump-tree-constbyterange-details" } */
/* { dg-final { scan-tree-dump-not "Constant byte cursor range" "constbyterange1" } } */
/* { dg-final { scan-tree-dump-not "Constant byte cursor range" "constbyterange2" } } */

/* A search supplies the seed, but p++ cannot close a fixed-displacement
   cursor cycle.  Pointer endpoint checks avoid a separately refinable
   load before the loop.  Weakening the seen-offset guard must fail this test.  */
static const char text[] = "#aa #bb #cc";
__attribute__((noipa)) static unsigned probe (unsigned start)
{
#ifdef PLAIN
  const char *p = text + start;
#else
  const char *p = __builtin_strchr (text + start, ' ');
#endif
  unsigned result = 0;
  if (p)
    while (p != text + sizeof (text) - 1)
      {
        unsigned char c = p[1];
        result += c == '#' ? c : c + 256;
        ++p;
      }
  return result;
}
__attribute__((noipa,optimize("O0"))) static unsigned expected (unsigned start)
{
#ifdef PLAIN
  const char *p = text + start;
#else
  const char *p = __builtin_strchr (text + start, ' ');
#endif
  unsigned result = 0;
  if (p)
    while (p != text + sizeof (text) - 1)
      {
        unsigned char c = p[1];
        result += c == '#' ? c : c + 256;
        ++p;
      }
  return result;
}
int main (void)
{
  for (unsigned i = 0; i != sizeof (text); ++i)
    if (probe (i) != expected (i))
      __builtin_abort ();
  return 0;
}
