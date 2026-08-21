/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-std=c++11 -O3 -march=armv8-a+sve -fdump-tree-optimized" } */

/* Control test: -ffind-with-sve is off by default, so the exact shape the
   feature rewrites (std::find over a vector of 64-bit integers, matched 8
   times in find-with-sve.C) must stay untouched without the option.  */

#include <algorithm>
#include <vector>
#include <cstdint>

std::uint64_t *probe (std::vector<std::uint64_t> &v, std::uint64_t x)
{
  auto it = std::find (v.begin (), v.end (), x);
  return it == v.end () ? nullptr : &*it;
}

/* { dg-final { scan-tree-dump-not "__sve_optimized_find_u64" "optimized" } } */
