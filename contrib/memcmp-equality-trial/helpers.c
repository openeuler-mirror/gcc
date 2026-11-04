/* Experimental, separately linked runtime helpers; no application edits. */
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#define ATTR __attribute__((noipa,visibility("hidden")))
static inline uint64_t u64(const void *p) { uint64_t x; __builtin_memcpy(&x,p,8); return x; }
static inline uint32_t u32(const void *p) { uint32_t x; __builtin_memcpy(&x,p,4); return x; }
static inline uint16_t u16(const void *p) { uint16_t x; __builtin_memcpy(&x,p,2); return x; }
ATTR int __gcc_trial_memcmpeq_bounded(const void *p,const void *q,size_t n)
{
  const unsigned char *a=p,*b=q;
  if (n>16) return memcmp(a,b,n);
  if (n>=8) return ((u64(a)^u64(b)) | (u64(a+n-8)^u64(b+n-8)))!=0;
  if (n>=4) return ((u32(a)^u32(b)) | (u32(a+n-4)^u32(b+n-4)))!=0;
  if (n>=2) return ((u16(a)^u16(b)) | (u16(a+n-2)^u16(b+n-2)))!=0;
  return n ? a[0]!=b[0] : 0;
}
ATTR int __gcc_trial_memcmpeq_loop(const void *p,const void *q,size_t n)
{
  const unsigned char *a=p,*b=q;
  for (size_t i=0;i<n;++i) if (a[i]!=b[i]) return 1;
  return 0;
}
