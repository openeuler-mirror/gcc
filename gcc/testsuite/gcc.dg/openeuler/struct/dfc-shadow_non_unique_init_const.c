/* { dg-do compile } */

#include <stdio.h>
#include <stdlib.h>

#define MAX 16

struct arc {
    unsigned long a;
    unsigned long b;
};
typedef struct arc arc_t;

arc_t* arcs;

int main() {
    arcs = calloc(MAX, sizeof(arc_t));
    char line[101];
    unsigned long a;
    unsigned long b;

    for (unsigned i = 0; i < MAX; i++) {
        arcs[i].a = 100;
    }

    for (unsigned i = 0; i < MAX; i++) {
        printf("a: %ld, b: %ld\n", arcs[i].a, arcs[i].b);
    }

    FILE* file = fopen("data.txt", "r");
    for (unsigned i = 0; i < MAX; i++) {
        fgets(line, 100, file);
        sscanf(line, "%ld %ld", &a, &b);
        arcs[i].a = a;
        arcs[i].b = a;
    }
    fclose(file);

    for (unsigned i = 0; i < MAX; i++) {
        printf("a: %ld, b: %ld\n", arcs[i].a, arcs[i].b);
    }

    // Should fail because of 100 is the init_const.
    for (unsigned i = 0; i < MAX; i++) {
        arcs[i].a = 100;
        arcs[i].b = 100;
    }

    for (unsigned i = 0; i < MAX; i++) {
        printf("a: %ld, b: %ld\n", arcs[i].a, arcs[i].b);
    }

    return 0;
}

/* { dg-final { scan-ipa-dump "\\\[field compress\\\] Init const is not unique" "struct_reorg" } } */
