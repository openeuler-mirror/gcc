/* { dg-do compile } */

#include <stdio.h>
#include <stdlib.h>

#define MAX 16

struct arc {
    unsigned long a;
    long b;
};
typedef struct arc arc_t;

arc_t* arcs;
arc_t* stop_arc;

int main() {
    arcs = calloc(MAX, sizeof(arc_t));
    stop_arc = arcs + MAX;

    char line[101];
    unsigned long a;
    long b;

    FILE* file = fopen("data.txt", "r");
    for (unsigned i = 0; i < MAX; i++) {
        fgets(line, 100, file);
        sscanf(line, "%ld %ld", &a, &b);
        arcs[i].a = a;
        arcs[i].b = b;
    }
    fclose(file);

    arc_t* arc = arcs;
    for (arc = arcs; arc != stop_arc; arc++) {
        // a = a + 1, Value of field a may be outside the closure, and we can't guarantee the validity of its boundary
        arc->a++;
        arc->b = arc->a;
    }

    for (arc = arcs; arc != stop_arc; arc++) {
        if (arc->a != arc->b)
            return 1;
    }

    return 0;
}

/* { dg-final { scan-ipa-dump "\\\[field compress\\\] Fail checking closure" "struct_reorg" } } */
