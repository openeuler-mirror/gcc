/* { dg-do run } */
/* { dg-options "-fno-ipa-immutable-clone-budget -fno-ipa-loop-value-specialization -fno-ipa-shared-loop-value-specialization -fno-ipa-bounded-cold-immutable-specialization -fno-ipa-immutable-helper-expansion -fipa-value-specialization -O3 -fno-inline -fipa-bounded-cold-immutable-specialization" } */
/* Explicit lengths and embedded zeroes must survive IPA substitution. */
__attribute__((noinline)) int inspect (const char *p, const unsigned char *d, unsigned n)
{
  if (!p) return -1;
  if (d[0] == 255) throw 37;
  unsigned result = 0;
  for (unsigned i=0;i<n;++i) result += (unsigned char)p[i] == d[i] ? 3 : 1;
  return result;
}
int dynamic(const char *p,const unsigned char *d,unsigned n)
{ return inspect(p,d,n); }
__attribute__((cold,noinline)) int fixed(const unsigned char *d,unsigned n)
{ return inspect("a\0b",d,n); }
const char *volatile unknown = "a\0b";
int main()
{
  unsigned state=123;
  for(unsigned j=0;j<16384;++j) {
    unsigned char d[4];
    for(unsigned i=0;i<4;++i) {state=state*1664525u+1013904223u;d[i]=state>>24;}
    unsigned n=j%5;
    int a=0,b=0;
    try {a=fixed(d,n);} catch(int x) {a=-x;}
    try {b=dynamic(unknown,d,n);} catch(int x) {b=-x;}
    if(a!=b) __builtin_abort();
  }
  unsigned char d[4]={255,0,0,0};
  try {fixed(d,4); __builtin_abort();} catch(int x) {if(x!=37)__builtin_abort();}
  return 0;
}
