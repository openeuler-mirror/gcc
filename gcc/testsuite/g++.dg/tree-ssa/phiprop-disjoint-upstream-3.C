/* { dg-do compile } */
/* { dg-options "-ftree-phiprop-disjoint-stores -O1 -fdump-tree-phiprop1-details -fdump-tree-release_ssa" } */

/* PR tree-optimization/123120 */
/* The order of min/max arguments should not cause any issues with phiprop,
   the stores to the temps should not interfere with the loads from the temp.  */

#include <algorithm>

long f0(short c, short d, long long a)
{
   return std::max(std::min((long long)3, a), (long long)d);
}

long f1(short c, short d, long long a)
{
   return std::max((long long)d, std::min((long long)3, a));
}

/* { dg-final { scan-tree-dump-times "Inserting PHI for result of load" 4 "phiprop1"} } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 2 "release_ssa"} } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 2 "release_ssa"} } */

