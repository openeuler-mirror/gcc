/* PR tree-optimization/122569 */
/* { dg-do compile } */
/* { dg-options "-fdebruijn-clz -O2 -fdump-tree-forwprop1-details" } */
/* { dg-additional-options "-mbmi" { target { x86_64-*-* && { ! ia32 } } } } */

/* Test that the forwprop DeBruijn CTZ matcher recognizes the 64-bit
   (x & -x) * magic >> 58 + table-lookup idiom used in Stockfish-style
   bitboard lsb() functions and many portable-C codebases that predate
   __builtin_ctzll.  */

typedef unsigned long long uint64_t;

static const int magictable[64] = {
   0,  1, 48,  2, 57, 49, 28,  3,
  61, 58, 50, 42, 38, 29, 17,  4,
  62, 55, 59, 36, 53, 51, 43, 22,
  45, 39, 33, 30, 24, 18, 12,  5,
  63, 47, 56, 27, 60, 41, 37, 16,
  54, 35, 52, 21, 44, 32, 23, 11,
  46, 26, 40, 15, 34, 20, 31, 10,
  25, 14, 19,  9, 13,  8,  7,  6
};

int
stockfish_lsb (uint64_t b)
{
  const uint64_t magic = 0x03f79d71b4cb0a89ULL;
  return magictable[((b & -b) * magic) >> 58];
}

/* The scan needs a zero result defined during GIMPLE, which the general
   ctzll effective target does not require.  Other targets still compile.  */
/* { dg-final { scan-tree-dump "__builtin_ctz|\\.CTZ" "forwprop1" { target { { ctzll && { lp64 || llp64 } } && { aarch64*-*-* || { x86_64-*-* && { ! ia32 } } } } } } } */
