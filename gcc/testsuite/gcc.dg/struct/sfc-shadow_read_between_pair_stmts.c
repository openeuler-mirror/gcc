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

__attribute__((__noinline__)) static void test_arc(arc_t* arcs, int i) {
    arcs[10].a = 10;
    arcs[10].b = 10;
    printf("%d, %d\n", arcs[i].a, arcs[i].b);

    /* sfc shadow check should fail here because of 
       reading arcs->a and arcs->b between pair assignment.  */
    arcs[10].a = 20;
    printf("%d, %d\n", arcs[i].a, arcs[i].b);
    arcs[10].b = 20;
}

int main() {
    arc_t* arcs = (arc_t*)calloc(MAX, sizeof(arc_t));
    for (int i = 0; i < MAX; i++) {
        arcs[i].a = i;
        arcs[i].b = i;
    }
    test_arc(arcs, 10);

    return 0;
}

/* { dg-final { scan-ipa-dump "\\\[field compress\\\] Fail finding shadow field" "struct_reorg" } } */
