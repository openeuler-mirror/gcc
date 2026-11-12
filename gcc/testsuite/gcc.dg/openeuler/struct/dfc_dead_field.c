/* { dg-do compile } */
/* { dg-additional-options "-fipa-struct-reorg=3" } */

#include <stdio.h>
#include <stdlib.h>

#define MAX 16

struct arc {
    unsigned long a;
    long b;
};
typedef struct arc arc_t;

arc_t* arcs;

int main() {
    arcs = calloc(MAX, sizeof(arc_t));
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

    for (unsigned i = 0; i < MAX; i++) {
        if (arcs[i].a != i)
            abort();
    }

    return 0;
}

/* { dg-final { scan-ipa-dump "\\\[field compress\\\] Found a dynamic compression field: a, input var: a" "struct_reorg" } } */
/* { dg-final { scan-ipa-dump "\\\[field compress\\\] Found a dynamic compression field: b, input var: b" "struct_reorg" { xfail *-*-* } } } */
