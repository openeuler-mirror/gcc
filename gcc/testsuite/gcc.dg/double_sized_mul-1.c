/* { dg-do compile } */
/* fif-conversion-gimple and fuaddsub-overflow-match-all are required for
   proper overflow detection in some cases.  */
/* { dg-options "-O2 -fif-conversion-gimple -fuaddsub-overflow-match-all -ftree-fold-phiopt -fdump-tree-widening_mul-stats" } */
#include <stdint.h>

typedef unsigned __int128 uint128_t;

uint16_t mul16 (uint8_t a, uint8_t b)
{
  uint8_t a_lo = a & 0xF;
  uint8_t b_lo = b & 0xF;
  uint8_t a_hi = a >> 4;
  uint8_t b_hi = b >> 4;
  uint8_t lolo = a_lo * b_lo;
  uint8_t lohi = a_lo * b_hi;
  uint8_t hilo = a_hi * b_lo;
  uint8_t hihi = a_hi * b_hi;
  uint8_t middle = hilo + lohi;
  uint8_t middle_hi = middle >> 4;
  uint8_t middle_lo = middle << 4;
  uint8_t res_lo = lolo + middle_lo;
  uint8_t res_hi = hihi + middle_hi;
  res_hi += (res_lo < middle_lo ? 1 : 0);
  res_hi += (middle < hilo ? 0x10 : 0);
  uint16_t res = ((uint16_t) res_hi) << 8;
  res += res_lo;
  return res;
}

uint32_t mul32 (uint16_t a, uint16_t b)
{
  uint16_t a_lo = a & 0xFF;
  uint16_t b_lo = b & 0xFF;
  uint16_t a_hi = a >> 8;
  uint16_t b_hi = b >> 8;
  uint16_t lolo = a_lo * b_lo;
  uint16_t lohi = a_lo * b_hi;
  uint16_t hilo = a_hi * b_lo;
  uint16_t hihi = a_hi * b_hi;
  uint16_t middle = hilo + lohi;
  uint16_t middle_hi = middle >> 8;
  uint16_t middle_lo = middle << 8;
  uint16_t res_lo = lolo + middle_lo;
  uint16_t res_hi = hihi + middle_hi;
  res_hi += (res_lo < middle_lo ? 1 : 0);
  res_hi += (middle < hilo ? 0x100 : 0);
  uint32_t res = ((uint32_t) res_hi) << 16;
  res += res_lo;
  return res;
}

uint64_t mul64 (uint32_t a, uint32_t b)
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
  return res;
}

uint128_t mul128 (uint64_t a, uint64_t b)
{
  uint64_t a_lo = a & 0xFFFFFFFF;
  uint64_t b_lo = b & 0xFFFFFFFF;
  uint64_t a_hi = a >> 32;
  uint64_t b_hi = b >> 32;
  uint64_t lolo = a_lo * b_lo;
  uint64_t lohi = a_lo * b_hi;
  uint64_t hilo = a_hi * b_lo;
  uint64_t hihi = a_hi * b_hi;
  uint64_t middle = hilo + lohi;
  uint64_t middle_hi = middle >> 32;
  uint64_t middle_lo = middle << 32;
  uint64_t res_lo = lolo + middle_lo;
  uint64_t res_hi = hihi + middle_hi;
  res_hi += (res_lo < middle_lo ? 1 : 0);
  res_hi += (middle < hilo ? 0x100000000 : 0);
  uint128_t res = ((uint128_t) res_hi) << 64;
  res += res_lo;
  return res;
}

uint64_t mul64_perm (uint32_t a, uint32_t b)
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
  res_hi = res_lo < middle_lo ? res_hi + 1 : res_hi;
  res_hi = middle < hilo ? res_hi + 0x10000 : res_hi;
  uint64_t res = ((uint64_t) res_hi) << 32;
  res += res_lo;
  return res;
}

uint128_t mul128_perm (uint64_t a, uint64_t b)
{
  uint64_t a_lo = a & 0xFFFFFFFF;
  uint64_t b_lo = b & 0xFFFFFFFF;
  uint64_t a_hi = a >> 32;
  uint64_t b_hi = b >> 32;
  uint64_t lolo = a_lo * b_lo;
  uint64_t lohi = a_lo * b_hi;
  uint64_t hilo = a_hi * b_lo;
  uint64_t hihi = a_hi * b_hi;
  uint64_t middle = hilo + lohi;
  uint64_t middle_hi = middle >> 32;
  uint64_t middle_lo = middle << 32;
  uint64_t res_lo = lolo + middle_lo;
  uint64_t res_hi = hihi + middle_hi;
  res_hi = res_lo < middle_lo ? res_hi + 1 : res_hi;
  res_hi = middle < hilo ? res_hi + 0x100000000 : res_hi;
  uint128_t res = ((uint128_t) res_hi) << 64;
  res += res_lo;
  return res;
}

/* { dg-final { scan-tree-dump-times "double sized mul optimized: 1" 6 "widening_mul" } } */
