/* { dg-do compile } */

struct Type_A;

// Optimized type.
struct Type_B {
    int b;
};

__attribute__((used)) static void test() {
    struct Type_A* a;
    struct Type_B* b;

    // MEM[(Type_B*)a] = *b;
    *((struct Type_B*)a) = *b;
}

// This testsuite should be compiled successfully without ICE.
