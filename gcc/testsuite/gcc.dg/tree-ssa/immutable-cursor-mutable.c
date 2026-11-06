/* { dg-do run } */
/* { dg-options "-O3 -fchecking=2 -ftree-immutable-loop-specialization-cursor" } */
/* A mutable stream can change while it is being interpreted.  */
static char stream[5] = "abcd";
static volatile unsigned reads;
__attribute__((noinline,noipa)) static int walk(const char *p, int change)
{
  unsigned sum=0;
  for (; *p; ++p) {
    sum+=(unsigned char)*p;
    ++reads;
    if (change && reads==2) stream[2]='x';
  }
  return sum;
}
int main(void)
{
  reads=0;
  if (walk(stream,1)!='a'+'b'+'x'+'d' || reads!=4) __builtin_abort();
  reads=0;
  if (walk("abcd",0)!='a'+'b'+'c'+'d' || reads!=4) __builtin_abort();
  return 0;
}
