/* { dg-do compile } */

#include <stdlib.h>

void __attribute__((__noinline__)) and_alignment_4(void *p) {
    if (((unsigned long)p + 3) & 3)
        abort();
}

int main() {
    int num;
    and_alignment_4(&num);

    return 0;
}

/*--------------------------------------------------------------------------*/
/* { dg-final { scan-ipa-dump "Rewrite" "alignment-propagation" { xfail *-*-* } } } */
