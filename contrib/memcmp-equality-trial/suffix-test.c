#include <stddef.h>
#include <stdlib.h>
#ifndef PREFIX
#define PREFIX 1
#endif
static const unsigned char *aa, *bb;
static unsigned calls;
int __wrap_memcmp(const void *a, const void *b, size_t n)
{
  if (a != aa + PREFIX || b != bb + PREFIX || n != 64 - PREFIX)
    abort();
  ++calls;
  const unsigned char *x=a, *y=b;
  for (size_t i=0;i<n;++i) if (x[i]!=y[i]) return (int)x[i]-(int)y[i];
  return 0;
}
__attribute__((noipa)) int ordered(const void *a,const void *b,size_t n)
{ return __builtin_memcmp(a,b,n); }
int main(void)
{
  unsigned char a[64]={0},b[64]={0}; aa=a;bb=b;
  b[63]=255;
  if (ordered(a,b,64)>=0 || calls!=1) abort();
  return 0;
}
