/* { dg-do run } */
/* { dg-options "-O3 -ftree-constant-byte-range-cursor -fdump-tree-constbyterange-details" } */
/* { dg-final { scan-tree-dump "Constant byte cursor range" "constbyterange1" } } */

#ifdef MUTABLE_TEXT
#define READONLY
#else
#define READONLY const
#endif
#ifndef CONTENT
#define CONTENT "#aa #bb #cc"
#endif
static READONLY char text[] = CONTENT;
__attribute__((noipa)) static unsigned probe (unsigned start)
{
  const char *p = __builtin_strchr (text + start, ' ');
  unsigned result = 0;
  while (p)
    {
      unsigned char c = p[1];
      result += c == '#' ? c : c + 256;
#ifdef CURSOR_LOOP
      p = __builtin_strchr (p + 1, ' ');
#else
      break;
#endif
    }
  return result;
}
__attribute__((noipa,optimize("O0"))) static unsigned expected (unsigned start)
{
  const char *p = __builtin_strchr (text + start, ' ');
  unsigned result = 0;
  while (p)
    {
      unsigned char c = p[1];
      result += c == '#' ? c : c + 256;
#ifdef CURSOR_LOOP
      p = __builtin_strchr (p + 1, ' ');
#else
      break;
#endif
    }
  return result;
}
int main (void)
{
  for (unsigned k = 0; k != 3; ++k)
    for (unsigned i = 0; i != sizeof(text); ++i)
      {
#ifdef MUTABLE_TEXT
	text[4] = k & 1 ? '$' : '#';
#endif
	if (probe(i) != expected(i)) __builtin_abort();
      }
  return 0;
}
