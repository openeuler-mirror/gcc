/* { dg-do compile } */
/* { dg-options "-O2 -ftree-constant-byte-range -fdump-tree-constbyterange-details" } */
/* { dg-final { scan-tree-dump-not "Constant byte object" "constbyterange1" } } */
/* { dg-final { scan-tree-dump-not "Constant byte object" "constbyterange2" } } */
char mutable_bytes[] = "abcxyz";
const volatile char volatile_bytes[] = "abcxyz";
const char *volatile loaded_pointer = "abcxyz";
int mutable_read (unsigned i) { return mutable_bytes[i]; }
int volatile_read (unsigned i) { return volatile_bytes[i]; }
int unknown_read (const char *p, unsigned i) { return p[i]; }
int loaded_read (unsigned i) { return loaded_pointer[i]; }
int mixed_read (unsigned i, int k)
{
  const char *p = k ? "abcxyz" : "012345";
  return p[i];
}
