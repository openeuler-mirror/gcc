/* { dg-do run } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-O2 -ftree-static-byte-cfg -fchecking=2 -fdump-tree-static-byte-cfg-details" } */

/* SUM stays dynamic and is not a tracked byte or an immutable address.
   COUNT keeps the inner loop dynamic; its PHIs and the join following the
   inner loop must survive state reuse and incremental SSA updating.  The
   unknown pointer also gives the copies ordinary virtual uses.  */
#define DEFINE_SCAN(NAME, ATTR)                                        \
__attribute__ ((noipa)) ATTR unsigned                                  \
NAME (unsigned key, unsigned count, const unsigned *value)             \
{                                                                    \
  const unsigned char *p = (const unsigned char *) "aBCdefGh";          \
  unsigned sum = key;                                                \
  while (*p)                                                         \
    {                                                                \
      for (unsigned j = 0; j < count; ++j)                            \
	sum = sum * 33 + ((key + j) ^ *p) + *value;                   \
      if (key & 1)                                                   \
	sum ^= key + *p;                                             \
      else                                                           \
	sum += key ^ *p;                                             \
      ++p;                                                           \
    }                                                                \
  return sum;                                                        \
}

DEFINE_SCAN (wide_phi, )
DEFINE_SCAN (reference, __attribute__ ((optimize ("no-tree-static-byte-cfg"))))

int
main (void)
{
  unsigned value = 71;
  for (unsigned key = 0; key < 300; ++key)
    for (unsigned count = 0; count < 4; ++count)
      if (wide_phi (key, count, &value) != reference (key, count, &value))
	__builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "GUARDPLAN wide_phi guarded=0 .*status=ready" "static-byte-cfg" } } */
/* { dg-final { scan-tree-dump-times "RESIDUAL-EMIT wide_phi " 1 "static-byte-cfg" } } */
/* { dg-final { scan-tree-dump "sum_.*PHI" "static-byte-cfg" } } */
/* { dg-final { scan-tree-dump-not "RESIDUAL-EMIT reference " "static-byte-cfg" } } */
