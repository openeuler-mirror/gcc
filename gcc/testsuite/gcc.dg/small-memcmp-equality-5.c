/* { dg-do run { target { aarch64*-*-linux* x86_64-*-linux* } } } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target non_strict_align } */
/* { dg-options "-O2 -finline-memcmp-small-equality -fdump-rtl-expand" } */
#include <sys/mman.h>
#include <unistd.h>
#include <stddef.h>

__attribute__((noipa)) int equal (const void *a, const void *b, size_t n)
{ return __builtin_memcmp (a, b, n) == 0; }

int main (void)
{
  size_t page = (size_t) sysconf (_SC_PAGESIZE);
  if (page < 128)
    __builtin_abort ();
  unsigned char *a = mmap (0, 2 * page, PROT_READ | PROT_WRITE,
                           MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  unsigned char *b = mmap (0, 2 * page, PROT_READ | PROT_WRITE,
                           MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (a == MAP_FAILED || b == MAP_FAILED
      || mprotect (a + page, page, PROT_NONE)
      || mprotect (b + page, page, PROT_NONE))
    __builtin_abort ();
  for (size_t n = 0; n != 97; ++n)
    {
      unsigned char *p = a + page - n, *q = b + page - n;
      for (size_t i = 0; i != n; ++i)
        p[i] = q[i] = (unsigned char) (i * 29);
      if (!equal (p, q, n))
        __builtin_abort ();
      for (size_t i = 0; i != n; ++i)
        {
          q[i] ^= 128;
          if (equal (p, q, n))
            __builtin_abort ();
          q[i] ^= 128;
        }
    }
  munmap (a, 2 * page);
  munmap (b, 2 * page);
  return 0;
}
/* { dg-final { scan-rtl-dump "Expanded bounded dynamic equality comparison" "expand" } } */
