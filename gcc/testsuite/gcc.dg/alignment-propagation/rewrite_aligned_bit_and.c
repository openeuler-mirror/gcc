/* { dg-do compile } */

#include <stdlib.h>

void __attribute__((__noinline__)) and_alignment_4(void *p) {
    if ((unsigned long)p & 3)
        abort();
}

void __attribute__((__noinline__)) and_alignment_8(void *p) {
    if ((unsigned long)p & 7)
        abort();
}

int main() {
    int num = 0;
    int nums[16] = {0};
    and_alignment_4(&num);
    and_alignment_4(nums);
    and_alignment_8(nums);

    return 0;
}

/*--------------------------------------------------------------------------*/
/* { dg-final { scan-ipa-dump-times "Rewrite" 2 "alignment-propagation" } } */
