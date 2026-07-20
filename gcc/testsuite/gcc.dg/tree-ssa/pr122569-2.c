/* PR tree-optimization/122569 */
/* { dg-do compile } */
/* { dg-options "-fdebruijn-clz -O2 -fdump-tree-forwprop1-details" } */

/* Test that the forwprop DeBruijn CLZ matcher recognizes the 64-bit
   OR-cascade + magic-multiply + table-lookup idiom used in
   Stockfish-style bitboard msb() functions and many portable-C
   codebases that predate __builtin_clzll.

   The critical edge case is magictable[63] == 63 (the MSB reached
   when the OR-cascade input has bit 63 set, so all 64 bits are set).
   The validator's CLZ checkfn computed (1 << (data + 1)) - 1 to
   simulate that all-ones value, which invoked shift-by-64 UB when
   data was 63, causing the transformation to be rejected for any
   valid 64-bit DeBruijn CLZ table.  */

typedef unsigned long long uint64_t;

static const int magictable[64] = {
   0, 47,  1, 56, 48, 27,  2, 60,
  57, 49, 41, 37, 28, 16,  3, 61,
  54, 58, 35, 52, 50, 42, 21, 44,
  38, 32, 29, 23, 17, 11,  4, 62,
  46, 55, 26, 59, 40, 36, 15, 53,
  34, 51, 20, 43, 31, 22, 10, 45,
  25, 39, 14, 33, 19, 30,  9, 24,
  13, 18,  8, 12,  7,  6,  5, 63
};

int
stockfish_msb (uint64_t b)
{
  const uint64_t magic = 0x03f79d71b4cb0a89ULL;
  b |= b >> 1;
  b |= b >> 2;
  b |= b >> 4;
  b |= b >> 8;
  b |= b >> 16;
  b |= b >> 32;
  return magictable[(b * magic) >> 58];
}

/* { dg-final { scan-tree-dump "__builtin_clz|\\.CLZ" "forwprop1" } } */
