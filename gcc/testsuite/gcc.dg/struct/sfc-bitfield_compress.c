/* { dg-do compile } */

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
        arcs[i].a = 4095;
        arcs[i].b = 8;
    }

    for (int i = 0; i < MAX; i++) {
        if (arcs[i].a != 4095 && arcs[i].b != 8)
            abort ();
    }

    return 0;
}

/* { dg-final { scan-ipa-dump "a {type = <unnamed-unsigned:12>}" "struct_reorg" } } */
/* { dg-final { scan-ipa-dump "b {type = <unnamed-unsigned:4>}" "struct_reorg" } } */
/* { dg-final { scan-ipa-dump "size : 2" "struct_reorg" } } */
