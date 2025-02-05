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
        arcs[i].a = i;
        arcs[i].b = i + 1;
    }

    for (int i = 0; i < MAX; i++) {
        if (arcs[i].a != 10000 && arcs[i].b != 10)
            abort ();
    }

    return 0;
}

/* { dg-final { scan-ipa-dump "\\\[field compress\\\] Fail finding static fc fields" "struct_reorg" } } */
