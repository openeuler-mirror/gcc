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
FILE *file;

void  __attribute__((noinline)) start_func() {
    arcs = calloc(MAX, sizeof(arc_t));
    char line[101];
    unsigned long a;
    long b;

    file = fopen("data.txt", "r");
    for (unsigned i = 0; i < MAX; i++) {
        fgets(line, 100, file);
        sscanf(line, "%ld %ld", &a, &b);
        arcs[i].a = a;
        arcs[i].b = b;
    }
}

int main() {
    start_func();
    fclose(file);

    for (unsigned i = 0; i < MAX; i++) {
        if (arcs[i].a != arcs[i].b)
            abort();
    }

    return 0;
}

/* { dg-final { scan-ipa-dump "\\\[field compress\\\] Fail finding fopen/fclose stmt" "struct_reorg" } } */
