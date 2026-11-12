/* { dg-do compile } */
/* { dg-require-effective-target bitint575 } */
/* { dg-options "-O2 -fchecking=2 -fno-tree-cselim -fdebruijn-clz" } */

/* These are not 32-bit half-word counts.  Reject their BLKmode inputs
   before asking for the scalar mode's defined zero result.  */
typedef unsigned _BitInt(256) U256;

int
clz_wide (int c, U256 x, U256 y)
{
  if (c)
    return 32 + __builtin_clzg (x, 256);
  return __builtin_clzg (y, 256);
}

int
ctz_wide (int c, U256 x, U256 y)
{
  if (c)
    return 32 + __builtin_ctzg (x, 256);
  return __builtin_ctzg (y, 256);
}
