/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fipa-cp-clone -fipa-value-specialization --param max-completely-peel-times=0 -fdump-tree-cunroll-details -fdump-ipa-cp-details" } */

/* The backing object's byte size is deliberately larger than the loop's
   precise upper bound.  IPA readonly-value peeling must not use the former as
   an unchecked peel count.  */

static const unsigned char input[16]
  = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };

__attribute__((noinline, noclone))
static int
opaque (int x)
{
  __asm__ ("" : "+r" (x));
  return x;
}

__attribute__((noinline))
static int
interpret_two (const unsigned char *cursor, int value)
{
  int result = value;
  for (int i = 0; i < 2; ++i)
    result = (result * 33) ^ *cursor++;
  return result;
}

int
test (int x)
{
  int result = 0;
  x = opaque (x);
  for (int i = 0; i < 200; ++i)
    result += interpret_two (input, x);
  return result;
}

/* The known pointer value must still create the IPA clone, but the 16-byte
   object must not request 16 copies of a loop whose upper bound is one.  */
/* { dg-final { scan-ipa-dump-times "Creating a specialized node of interpret_two" 1 "cp" } } */
/* { dg-final { scan-tree-dump-times "Not peeling IPA readonly-value loop: requested 16 copies exceed known upper bound" 1 "cunroll" } } */
