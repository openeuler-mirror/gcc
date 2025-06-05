/* { dg-do compile } */

#include <stdio.h>
#include <stdlib.h>

static int* p;
int** a;

void __attribute__((noinline)) test() {
    a = &p;
}

/* Set -O0 so that the ssa define by calloc and used by free
   are not the same one.  */
#pragma GCC push_options
#pragma GCC optimize("O0")
int main() {
    p = calloc(10, sizeof(int));
    test();
    int ret = **a;
    free(p);
    return ret;
}
#pragma GCC pop_options

/*--------------------------------------------------------------------------*/
/* { dg-final { scan-ipa-dump "Localize global array" "localize-array" { xfail *-*-* } } } */
