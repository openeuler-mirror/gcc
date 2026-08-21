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
        arcs[i].b = 10000000000L;
    }

    for (int i = 0; i < MAX; i++) {
        if (arcs[i].b != 10000000000L)
            abort ();
    }

    return 0;
}

/* { dg-final { scan-ipa-dump "\\\[field compress\\\] Max value of b is too big, max_value = 10000000000" "struct_reorg" } } */
