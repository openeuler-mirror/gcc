/* { dg-do run } */
/* { dg-options "-O3 -ftree-immutable-cursor-specialization" } */
static volatile int seen;
__attribute__((noinline,noipa)) static int effect(int x)
{ ++seen; if(x==2) throw x; return x; }
__attribute__((noinline,noipa)) static int walk(int stop)
{
  int sum=0;
  for (const char *p="1234"; *p; ++p)
    try { sum+=effect(*p-'0'==stop ? 2 : 1); }
    catch (int x) { sum+=x*10; }
  return sum;
}
int main()
{ if(walk(3)!=23 || seen!=4) __builtin_abort(); }
