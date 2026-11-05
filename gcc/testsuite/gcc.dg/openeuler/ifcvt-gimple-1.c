/* { dg-do run } */
/* { dg-options "-O2 -fno-inline -fif-conversion-gimple" } */

#include <stdlib.h>

void foo(int a, int *p) {
    *p = a;
}

void verify (int a) {
    if (a != 3)
        abort ();
}

int main() {
    int a = 0;
    foo (3, &a);
    int tmp = (a > 7) ? a & 1 : a;
    verify (tmp);
    return 0;
}
