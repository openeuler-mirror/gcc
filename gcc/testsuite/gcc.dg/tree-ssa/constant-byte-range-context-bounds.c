/* { dg-do compile } */
/* { dg-options "-O2 -Wall -ftree-constant-byte-range" } */
static const char zeros[] = "\0\0";
int f (unsigned i) { if (i >= 3) return zeros[i]; return 1; } /* { dg-warning "array subscript 3 is above array bounds" } */
int g (unsigned i) { const char *p = zeros; if (i >= 3) return p[i]; return 1; } /* { dg-warning "array subscript 3 is outside array bounds" } */
