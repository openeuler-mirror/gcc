/* { dg-do compile { target aarch64*-*-* x86_64*-*-*} } */
/* fif-conversion-gimple and fuaddsub-overflow-match-all are required for
   proper overflow detection in some cases.  */
/* { dg-options "-O2 -fif-conversion-gimple -fuaddsub-overflow-match-all -fdump-tree-widening_mul-stats" } */
/* { dg-additional-options "-march=armv8.2-a" { target aarch64*-*-* } } */
#include <stdint.h>

typedef unsigned __int128 uint128_t;
typedef struct uint256_t
{
    uint128_t lo;
    uint128_t hi;
} uint256_t;

uint64_t mul64_double_use (uint32_t a, uint32_t b)
{
  uint32_t a_lo = a & 0xFFFF;
  uint32_t b_lo = b & 0xFFFF;
  uint32_t a_hi = a >> 16;
  uint32_t b_hi = b >> 16;
  uint32_t lolo = a_lo * b_lo;
  uint32_t lohi = a_lo * b_hi;
  uint32_t hilo = a_hi * b_lo;
  uint32_t hihi = a_hi * b_hi;
  uint32_t middle = hilo + lohi;
  uint32_t middle_hi = middle >> 16;
  uint32_t middle_lo = middle << 16;
  uint32_t res_lo = lolo + middle_lo;
  uint32_t res_hi = hihi + middle_hi;
  res_hi += (res_lo < middle_lo ? 1 : 0);
  res_hi += (middle < hilo ? 0x10000 : 0);
  uint64_t res = ((uint64_t) res_hi) << 32;
  res += res_lo;
  return res + lolo;
}

uint256_t mul256 (uint128_t a, uint128_t b)
{
  uint128_t a_lo = a & 0xFFFFFFFFFFFFFFFF;
  uint128_t b_lo = b & 0xFFFFFFFFFFFFFFFF;
  uint128_t a_hi = a >> 64;
  uint128_t b_hi = b >> 64;
  uint128_t lolo = a_lo * b_lo;
  uint128_t lohi = a_lo * b_hi;
  uint128_t hilo = a_hi * b_lo;
  uint128_t hihi = a_hi * b_hi;
  uint128_t middle = hilo + lohi;
  uint128_t middle_hi = middle >> 64;
  uint128_t middle_lo = middle << 64;
  uint128_t res_lo = lolo + middle_lo;
  uint128_t res_hi = hihi + middle_hi;
  res_hi += (res_lo < middle_lo ? 1 : 0);
  /* Constant is to big warning WA */
  uint128_t overflow_tmp = (middle < hilo ? 1 : 0);
  overflow_tmp <<= 64;
  res_hi += overflow_tmp;
  uint256_t res;
  res.lo = res_lo;
  res.hi = res_hi;
  return res;
}

/* { dg-final { scan-tree-dump-not "double sized mul optimized" "widening_mul" } } */
