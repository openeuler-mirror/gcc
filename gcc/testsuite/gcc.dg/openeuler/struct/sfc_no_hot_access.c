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
    arcs[10].a = 10000;

    for (int i = 0; i < MAX; i++) {
        if (arcs[i].a != 10000)
            abort ();
    }

    return 0;
}

/* { dg-final { scan-ipa-dump "\\\[field compress\\\] Fail finding hot access for static" "struct_reorg" } } */
