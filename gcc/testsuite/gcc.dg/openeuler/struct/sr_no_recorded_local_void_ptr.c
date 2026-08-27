/* { dg-do compile } */
/* { dg-additional-options "-fgimple" } */

#include <stdlib.h>

struct S {
    int a;
    int b;
};

__attribute__((noinline)) void __GIMPLE(startwith("struct_reorg")) test() {
    void* ptr;

    ptr = calloc(1UL, 8UL);
    __MEM <struct S> ((struct S*)ptr).a = 0;
}

int main() {
    test();
    return 0;
}

/* { dg-final { scan-ipa-dump "struct S has escaped: \"Type escapes via no record var\"" "struct_reorg" } } */
