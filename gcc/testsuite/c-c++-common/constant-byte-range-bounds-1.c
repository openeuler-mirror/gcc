/* Preserve diagnostics before assigning ranges to constant byte loads.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Warray-bounds -ftree-constant-byte-range" } */

void sink (int, ...);

void
empty_indices (void)
{
  sink (""[-1]); /* { dg-warning "array subscript" } */
  sink (""[1]); /* { dg-warning "array subscript" } */
  sink (""[-__PTRDIFF_MAX__]); /* { dg-warning "array subscript" } */
  sink (""[__PTRDIFF_MAX__ - 1]); /* { dg-warning "array subscript" } */
}

void
mem_ref_indices (void)
{
  static const char empty[] = "";
  const char *p = empty;
  sink (p[-1]); /* { dg-warning "array subscript" } */
  sink (p[1]); /* { dg-warning "array subscript" } */
}
