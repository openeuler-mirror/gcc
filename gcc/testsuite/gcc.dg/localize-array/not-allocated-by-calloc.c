/* { dg-do compile } */

#include <stdio.h>
#include <stdlib.h>

static int* p;

void __attribute__((noinline)) test() {
    for (unsigned i = 0; i < 10; i++) {
        p[i] = i;
    }
}

/* Set -O0 so that the ssa define by calloc and used by free
   are not the same one.  */
#pragma GCC push_options
#pragma GCC optimize("O0")
int main() {
    p = malloc(10 * sizeof(int));
    test();
    free(p);
    return 0;
}
#pragma GCC pop_options

/*--------------------------------------------------------------------------*/
/* { dg-final { scan-ipa-dump "Localize global array" "localize-array" { xfail *-*-* } } } */
