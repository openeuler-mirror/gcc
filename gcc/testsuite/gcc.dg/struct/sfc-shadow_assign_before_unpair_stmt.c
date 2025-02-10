/* { dg-do compile } */

#include <stdio.h>
#include <stdlib.h>

struct arc {
    unsigned long a;
    unsigned long b;
    int c;
};
typedef struct arc arc_t;

#define MAX 16

int main() {
    arc_t* arcs = (arc_t*)calloc(MAX, sizeof(arc_t));
    for (int i = 0; i < MAX; i++) {
        arcs[i].a = i;
        arcs[i].b = i;
    }
    for (int i = 0; i < MAX; i++) {
        arcs[i].a = 20;
    }
    printf("%d, %d\n", arcs[10].a, arcs[10].b);

    return 0;
}

/* { dg-final { scan-ipa-dump "\\\[field compress\\\] Fail finding shadow field" "struct_reorg" } } */
