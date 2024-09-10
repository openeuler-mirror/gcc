/* { dg-do compile { target aarch64*-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fuaddsub-overflow-match-all -fdump-tree-optimized" } */
/* { dg-additional-options "-march=armv8.2-a" { target aarch64*-*-* } } */
#include <stdint.h>

typedef unsigned __int128 uint128_t;
typedef struct uint256_t
{
  uint128_t lo;
  uint128_t hi;
} uint256_t;

uint16_t add16 (uint8_t a, uint8_t b)
{
  uint8_t tmp = a + b;
  uint8_t overflow = 0;
  if (tmp < a)
    overflow = 1;

  uint16_t res = overflow;
  res <<= 8;
  res += tmp;
  return res;
}

uint32_t add32 (uint16_t a, uint16_t b)
{
  uint16_t tmp = a + b;
  uint16_t overflow = 0;
  if (tmp < a)
    overflow = 1;

  uint32_t res = overflow;
  res <<= 16;
  res += tmp;
  return res;
}

uint64_t add64 (uint32_t a, uint32_t b)
{
  uint32_t tmp = a + b;
  uint32_t overflow = 0;
  if (tmp < a)
    overflow = 1;

  uint64_t res = overflow;
  res <<= 32;
  res += tmp;
  return res;
}

uint128_t add128 (uint64_t a, uint64_t b)
{
  uint64_t tmp = a + b;
  uint64_t overflow = 0;
  if (tmp < a)
    overflow = 1;

  uint128_t res = overflow;
  res <<= 64;
  res += tmp;
  return res;
}

uint256_t add256 (uint128_t a, uint128_t b)
{
  uint128_t tmp = a + b;
  uint128_t overflow = 0;
  if (tmp < a)
    overflow = 1;

  uint256_t res;
  res.hi = overflow;
  res.lo = tmp;
  return res;
}

uint16_t sub16 (uint8_t a, uint8_t b)
{
  uint8_t tmp = a - b;
  uint8_t overflow = 0;
  if (tmp > a)
    overflow = -1;

  uint16_t res = overflow;
  res <<= 8;
  res += tmp;
  return res;
}

uint32_t sub32 (uint16_t a, uint16_t b)
{
  uint16_t tmp = a - b;
  uint16_t overflow = 0;
  if (tmp > a)
    overflow = -1;

  uint32_t res = overflow;
  res <<= 16;
  res += tmp;
  return res;
}

uint64_t sub64 (uint32_t a, uint32_t b)
{
  uint32_t tmp = a - b;
  uint32_t overflow = 0;
  if (tmp > a)
    overflow = -1;

  uint64_t res = overflow;
  res <<= 32;
  res += tmp;
  return res;
}

uint128_t sub128 (uint64_t a, uint64_t b)
{
  uint64_t tmp = a - b;
  uint64_t overflow = 0;
  if (tmp > a)
    overflow = -1;

  uint128_t res = overflow;
  res <<= 64;
  res += tmp;
  return res;
}

uint256_t sub256 (uint128_t a, uint128_t b)
{
  uint128_t tmp = a - b;
  uint128_t overflow = 0;
  if (tmp > a)
    overflow = -1;

  uint256_t res;
  res.hi = overflow;
  res.lo = tmp;
  return res;
}

/* { dg-final { scan-tree-dump-times "= .ADD_OVERFLOW \\(a_\[0-9\]+\\(D\\), b_\[0-9\]+\\(D\\)\\)" 5 "optimized" } } */
/* { dg-final { scan-tree-dump-times "= .SUB_OVERFLOW \\(a_\[0-9\]+\\(D\\), b_\[0-9\]+\\(D\\)\\)" 5 "optimized" { target aarch64*-*-* } } } */
/* { dg-final { scan-tree-dump-times "= .SUB_OVERFLOW \\(a_\[0-9\]+\\(D\\), b_\[0-9\]+\\(D\\)\\)" 4 "optimized" { target x86_64*-*-* } } } */
