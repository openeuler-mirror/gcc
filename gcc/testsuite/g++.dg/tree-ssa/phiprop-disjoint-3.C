/* { dg-do run } */
/* { dg-options "-O3 -fno-early-inlining -ftree-phiprop-disjoint-stores" } */
template <class T> const T& minref (const T& a, const T& b)
{ return b < a ? b : a; }
template <class T> const T& maxref (const T& a, const T& b)
{ return b > a ? b : a; }

#define DEFINE(NAME,ATTR) \
__attribute__((noinline,noclone)) ATTR \
void NAME (const int *p, unsigned n, int seed, int limit, int *out) \
{ \
 int low=seed, high=seed; \
 for(unsigned i=0;i<n;++i) \
  { int x=p[i]; if(x<low) low=x; else if(x>high) high=x; } \
 out[0]=maxref(minref(low,limit),1); \
 out[1]=maxref(minref(high,limit),1); \
}
DEFINE (optimized, )
DEFINE (reference, __attribute__((optimize("no-tree-phiprop-disjoint-stores"))))

__attribute__((noinline,noclone)) void change (int *p) { *p ^= 17; }
#define ALIAS(NAME,ATTR) \
__attribute__((noinline,noclone)) ATTR int NAME(int a,int b,bool choose) \
{ int *p=choose?&a:&b; change(&a); return *p; }
ALIAS(alias_on,)
ALIAS(alias_off,__attribute__((optimize("no-tree-phiprop-disjoint-stores"))))

int main ()
{
 int data[129], a[2], b[2]; unsigned state=1;
 const int bounds[]={(-__INT_MAX__-1),-1,0,1,__INT_MAX__};
 for(unsigned trial=0;trial<30;++trial)
  {
   for(unsigned i=0;i<129;++i)
     { state=state*1664525u+1013904223u; data[i]=(int)state; }
   data[0]=(-__INT_MAX__-1); data[1]=__INT_MAX__;
   for(unsigned n=0;n<=129;++n)
    for(unsigned k=0;k<5;++k)
     {
      optimized(data,n,(int)state,bounds[k],a);
      reference(data,n,(int)state,bounds[k],b);
      if(a[0]!=b[0] || a[1]!=b[1]) __builtin_abort();
      for(unsigned c=0;c<2;++c)
       if(alias_on((int)state,bounds[k],c)!=alias_off((int)state,bounds[k],c))
        __builtin_abort();
     }
  }
}

