/* { dg-do compile } */
/* { dg-options "-O2 -Warray-bounds -Wstringop-overread -ftree-constant-byte-range" } */
static const char t[] = "abc";
const char *g_const (void) { return __builtin_strchr (t + 5, 'x'); } /* { dg-warning "outside bounds of constant string" } */
/* { dg-warning "array subscript 5 is outside array bounds" "" { target *-*-* } .-1 } */
const char *g_ctx (int i) { if (i > 4) return __builtin_strchr (t + i, 'x'); return 0; } /* { dg-warning "reading 1 or more bytes from a region of size 0" } */
