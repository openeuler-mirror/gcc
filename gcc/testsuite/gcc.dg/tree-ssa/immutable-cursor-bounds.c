/* { dg-do run } */
/* { dg-options "-O3 -ftree-immutable-loop-specialization-cursor" } */
static const unsigned char bytes[]={3,0,255,7};
__attribute__((noinline,noipa)) static unsigned walk(unsigned n)
{
  unsigned sum=0;
  const unsigned char *p=bytes;
  while (p!=bytes+n) { sum+=*p; ++p; }
  return sum;
}
int main(void)
{ if(walk(0)||walk(1)!=3||walk(2)!=3||walk(3)!=258||walk(4)!=265) __builtin_abort(); }
