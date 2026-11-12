/* { dg-do compile } */
/* { dg-options "-O3 -fipa-localize-array -fdump-ipa-localize-array-details -fwhole-program" } */

#include <stdio.h>
#include <stdlib.h>

static int* p;

void __attribute__((noinline)) test() {
    p[0] = 0;
}

/* Set -O0 so that the ssa define by calloc and used by free
   are not the same one.  */
#pragma GCC push_options
#pragma GCC optimize("O0")
int main() {
    p = calloc(1, sizeof(int));
    test();
    free(p);
    return 0;
}
#pragma GCC pop_options

/*--------------------------------------------------------------------------*/
/* { dg-final { scan-ipa-dump "Localize global array: p" "localize-array" } } */
/* { dg-final { scan-ipa-dump "Insert calloc statement" "localize-array" { xfail *-*-* } } } */
