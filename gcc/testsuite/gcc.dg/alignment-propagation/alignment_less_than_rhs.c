/* { dg-do compile } */

#include <stdlib.h>

void __attribute__((__noinline__)) and_alignment_128(void *p) {
    if ((unsigned long)p & 127)
        abort();
}

int main() {
    int num[16];
    and_alignment_128(num);

    return 0;
}

/*--------------------------------------------------------------------------*/
/* { dg-final { scan-ipa-dump "Rewrite" "alignment-propagation" { xfail *-*-* } } } */
