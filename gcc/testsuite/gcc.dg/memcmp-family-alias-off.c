/* { dg-do run } */
/* { dg-options "-O2 -finline-memcmp -fno-inline-small-memcmp-equality" } */
#include <string.h>
__attribute__((noinline,noclone)) int cmp(const char *a,const char *b,unsigned n) { return memcmp(a,b,n); }
int main(void) { const char a[]="abcdef", b[]="abcxef"; for(unsigned n=0;n<=6;++n) { int r=cmp(a,b,n); if ((n<=3 && r!=0) || (n>3 && r>=0)) __builtin_abort(); } return 0; }
