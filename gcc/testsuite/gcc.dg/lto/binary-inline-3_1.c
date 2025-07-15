/* { dg-options "-O2 -fno-math-errno" }  */

#include <math.h>

double multi_op (double x)
{
    double a = 0;
    a = sqrt (x);
    return a * 2 + 10;
}
