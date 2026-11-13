#include <sys/mman.h>
#include <unistd.h>
#include <stdlib.h>
#include <stddef.h>
__attribute__((noipa)) int ord(const void *a,const void *b,size_t n)
{ return __builtin_memcmp(a,b,n); }
__attribute__((noipa)) int eq(const void *a,const void *b,size_t n)
{ return __builtin_memcmp(a,b,n)==0; }
int main(void)
{
  size_t page=sysconf(_SC_PAGESIZE);
  unsigned char *a=mmap(0,3*page,PROT_NONE,MAP_PRIVATE|MAP_ANONYMOUS,-1,0);
  unsigned char *b=mmap(0,3*page,PROT_NONE,MAP_PRIVATE|MAP_ANONYMOUS,-1,0);
  if(a==MAP_FAILED || b==MAP_FAILED || mprotect(a+page,page,PROT_READ|PROT_WRITE)
     || mprotect(b+page,page,PROT_READ|PROT_WRITE)) abort();
  for(size_t n=0;n<=257;++n)
    for(unsigned at_end=0;at_end<2;++at_end) {
      unsigned char *x=a+page+(at_end?page-n:0),*y=b+page+(at_end?page-n:0);
      for(size_t j=0;j<n;++j) x[j]=y[j]=(unsigned char)(j*83+193);
      if(ord(x,y,n)!=0 || !eq(x,y,n)) abort();
      for(size_t j=0;j<n;++j) {
	y[j]^=128;
	int sign=x[j]<y[j]?-1:1,v=ord(x,y,n);
	if((v<0?-1:v>0)!=sign || eq(x,y,n)) abort();
	y[j]^=128;
      }
    }
  if(ord(a,b,0)!=0 || !eq(a,b,0)) abort();
  return 0;
}
