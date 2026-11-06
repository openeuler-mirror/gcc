/* { dg-do run } */
/* { dg-options "-O3 -fchecking=2 -ftree-immutable-loop-specialization-cursor" } */
static inline __attribute__((always_inline)) unsigned walk(const char *p)
{
  unsigned r=0;
  while (*p) { if (*p=='a') r+=7; else r+=(unsigned char)*p; ++p; }
  return r;
}
__attribute__((noinline,noipa)) static unsigned choose(int c,const char *p)
{ return walk(c ? "abc" : p); }
int main(void)
{
  char buffer[4]={'x','y','z',0};
  asm volatile("" : "+m"(buffer));
  if (choose(1,buffer)!=7+'b'+'c') __builtin_abort();
  if (choose(0,buffer)!='x'+'y'+'z') __builtin_abort();
  buffer[0]='a';
  if (choose(0,buffer)!=7+'y'+'z') __builtin_abort();
  return 0;
}
