/* { dg-do compile } */

#include <stdio.h>
#include <stdlib.h>

#define MAX 16

struct arc {
    unsigned long a;
    unsigned long b;
    int c;
};
typedef struct arc arc_t;

arc_t* arcs;

int main() {
    arcs = calloc(MAX, sizeof(arc_t));
    char line[101];
    int c;

    for (unsigned i = 0; i < MAX; i++) {
        arcs[i].a = 100;
    }

    FILE* file = fopen("data.txt", "r");
    for (unsigned i = 0; i < MAX; i++) {
        fgets(line, 100, file);
        sscanf(line, "%ld", &c);
        arcs[i].a = i;
        arcs[i].b = i;
        arcs[i].c = c;
    }
    fclose(file);

    for (unsigned i = 0; i < MAX; i++) {
        printf("a: %ld, b: %ld\n", arcs[i].a, arcs[i].b);
    }

    return 0;
}

/* { dg-final { scan-ipa-dump "\\\[field compress\\\] Fail checking dynamic shadow fields" "struct_reorg" } } */
