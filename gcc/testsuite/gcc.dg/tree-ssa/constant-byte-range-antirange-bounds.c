/* Preserve a diagnostic when both disjoint tails are out of bounds.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Warray-bounds -ftree-constant-byte-range" } */
static const char z[] = "\0\0";
int f (int i)
{
  if (i >= 0 && i < (int) sizeof z)
    return 1;
  return z[i]; /* { dg-warning "array subscript.*bounds" } */
}
