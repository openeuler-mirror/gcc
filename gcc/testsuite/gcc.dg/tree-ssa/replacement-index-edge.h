/* Shared direct-edge examples.  Keep the tail loop so the existing
   enclosing loop-pass gate admits the function.  */
#include <stdint.h>
__attribute__((noipa)) unsigned
edge_read32 (unsigned *q, const unsigned *t, unsigned next,
	     unsigned other, unsigned choose, unsigned n)
{
  unsigned v;
  if (choose) { v = *q; *q = next; } else v = other;
  unsigned tail = 0;
  for (unsigned i = 0; i < n; ++i) tail += t[i & 15];
  return t[v & 255] + tail;
}
__attribute__((noipa)) uint64_t
edge_read64 (uint64_t *q, const unsigned *t, uint64_t next,
	     uint64_t other, unsigned choose, unsigned n)
{
  uint64_t v;
  if (choose) { v = *q; *q = next; } else v = other;
  uint64_t tail = 0;
  for (unsigned i = 0; i < n; ++i) tail += t[i & 15];
  return t[v & 255] + tail;
}
/* Two indirect addresses: the new entry selects at most one exact hint.  */
__attribute__((noipa)) unsigned
edge_store (unsigned *q, unsigned *t, const unsigned *s, unsigned next,
	    unsigned other, unsigned choose, unsigned n)
{
  unsigned v;
  if (choose) { v = *q; *q = next; } else v = other;
  unsigned x = s[v & 255];
  t[(v >> 8) & 255] = v;
  unsigned tail = 0;
  for (unsigned i = 0; i < n; ++i) tail += t[i & 15];
  return x + tail;
}
