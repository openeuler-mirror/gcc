/* { dg-do compile } */

#include <stdio.h>
#include <stdlib.h>

#define MAX 16

struct arc {
    unsigned long a;
    long b;
};
typedef struct arc arc_t;

arc_t* arcs1;
arc_t* arcs2;

int main() {
    arcs1 = calloc(MAX, sizeof(arc_t));
    arcs2 = arcs1;
    char line[101];
    unsigned long a;
    long b;

    FILE* file = fopen("data.txt", "r");
    for (unsigned i = 0; i < MAX; i++) {
        fgets(line, 100, file);
        sscanf(line, "%ld %ld", &a, &b);
        arcs1[i].a = a;
        arcs1[i].b = b;
    }
    fclose(file);

    for (unsigned i = 0; i < MAX; i++) {
        if (arcs1[i].a != arcs2[i].b)
            return 1;
    }

    return 0;
}

/* { dg-final { scan-ipa-dump "\\\[field compress\\\] Array assigned to multiple variable" "struct_reorg" } } */
