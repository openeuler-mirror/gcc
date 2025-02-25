/* { dg-do compile } */
/* { dg-additional-options "-fipa-struct-reorg=3" } */

#include <stdio.h>
#include <stdlib.h>

struct arc {
    unsigned long a;
    unsigned long b;
};
typedef struct arc arc_t;

#define MAX 16

int main() {
    arc_t* arcs = (arc_t*)calloc(MAX, sizeof(arc_t));
    for (int i = 0; i < MAX; i++) {
        arcs[i].a = 10000;
        arcs[i].b = 10;
    }

    for (int i = 0; i < MAX; i++) {
        if (arcs[i].a != 10000)
            abort ();
    }

    return 0;
}

/* { dg-final { scan-ipa-dump "\\\[field compress\\\] Found a static compression field: a, max_value = 10000" "struct_reorg" } } */
/* { dg-final { scan-ipa-dump "\\\[field compress\\\] Found a static compression field: b" "struct_reorg" { xfail *-*-* } } } */
/* { dg-final { scan-ipa-dump "size : 2" "struct_reorg" } } */
