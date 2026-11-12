/* -farray-widen-compare is documented as unsafe: the widened loop reads
   up to eight bytes past the first mismatching byte, which the original
   loop never touches.  This program is correct as written - the mismatch
   is the last readable byte before a PROT_NONE page and the byte-wise
   loop breaks there - so it must crash only because of the rewrite.

   The crash is the recorded, expected outcome (xfail).  If this ever
   XPASSes, the unsafe read is gone - either the rewrite became safe or
   recognition was lost; the dump scan below distinguishes the two.  */
/* { dg-do run { target {{ aarch64*-*-linux* } && lp64 } xfail *-*-* } } */
/* { dg-options "-O3 -farray-widen-compare -fdump-tree-awiden_compare-details" } */

#include <stdint.h>
#include <string.h>
#include <sys/mman.h>

#define my_min(x, y) ((x) < (y) ? (x) : (y))

__attribute__ ((noipa)) uint32_t
func (uint32_t len0, uint32_t len1, const uint32_t len_limit,
      const uint8_t *const pb, const uint8_t *const cur)
{
  uint32_t len = my_min (len0, len1);
  while (++len != len_limit)
    if (pb[len] != cur[len])
      break;
  return len;
}

int
main (void)
{
  const long pg = 4096;
  uint8_t *m1 = (uint8_t *) mmap (0, 3 * pg, PROT_READ | PROT_WRITE,
				  MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  uint8_t *m2 = (uint8_t *) mmap (0, 3 * pg, PROT_READ | PROT_WRITE,
				  MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  /* Environment failure also lands on xfail; there is no way to tell
     dejagnu "unresolved" from here.  */
  if (m1 == MAP_FAILED || m2 == MAP_FAILED
      || mprotect (m1 + 2 * pg, pg, PROT_NONE) != 0
      || mprotect (m2 + 2 * pg, pg, PROT_NONE) != 0)
    return 2;

  /* Both buffers end at the very last readable byte; the first mismatch
     sits on that byte, the loop bound beyond it.  */
  const uint32_t n = 300;
  uint8_t *a = m1 + 2 * pg - n;
  uint8_t *b = m2 + 2 * pg - n;
  memset (a, 7, n);
  memset (b, 7, n);
  a[n - 1] = 9;

  return func (0, 0, n + 64, a, b) == n - 1 ? 0 : 1;
}

/* { dg-final { scan-tree-dump-times "loop form is success" 1 "awiden_compare" } } */
