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
    arc_t* arcs1 = (arc_t*)calloc(MAX, sizeof(arc_t));
    arc_t* arcs2 = (arc_t*)calloc(MAX, sizeof(arc_t));
    for (int i = 0; i < MAX; i++) {
        arcs1[i].a = i;
        arcs1[i].b = i;
    }
    for (int i = 0; i < MAX; i++) {
        arcs2[i].b = 0;
    }
    printf("%d, %d\n", arcs1[10].a, arcs1[10].b);
    printf("%d, %d\n", arcs2[10].a, arcs2[10].b);

    return 0;
}

/* { dg-final { scan-ipa-dump "\\\[field compress\\\] Found shadow field: a" "struct_reorg" } } */
