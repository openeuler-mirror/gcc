/* { dg-do compile } */
/* { dg-options "-fgimple -O1 -std=gnu23" } */
/* -std=gnu23 for the bool keyword; gcc-14 defaults to gnu17.  */
#pragma GCC target "+cmpbr"
typedef unsigned short us;
__GIMPLE double
f (us x, us y, double a, double b)
{
  bool c;
  double d;
  c = x == y;
  d = c ? a : b;
  return d;
}
