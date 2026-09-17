/* { dg-do compile } */
/* { dg-options "-std=c++11 -O3 -ffind-with-sve -march=armv8-a+sve -fdump-tree-optimized" } */

#include <algorithm>
#include <cstdint>

unsigned char *
test_u8_find_u64 (unsigned char *first, unsigned char *last,
		  const std::uint64_t &value)
{
  return std::find (first, last, value);
}

/* { dg-final { scan-tree-dump-not "__sve_optimized_find_u64" "optimized" } } */
