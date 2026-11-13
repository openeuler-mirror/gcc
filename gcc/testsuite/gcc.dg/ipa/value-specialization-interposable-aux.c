/* { dg-do compile } */
/* { dg-options "-O3 -fPIC -fsemantic-interposition -fipa-value-specialization" } */
/* { dg-require-effective-target fpic } */
/* { dg-require-weak "" } */
__attribute__((weak)) const unsigned char interposed_program[4] = {1, 2, 0};
__attribute__((noinline)) static unsigned decode (const unsigned char *p, unsigned x)
{
  unsigned result = x;
  while (*p) { result = result * 3 + *p; ++p; }
  return result;
}
unsigned read_interposed (unsigned x) { return decode (interposed_program, x); }
