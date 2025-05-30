/* { dg-do compile } */
/* { dg-additional-options "-fgimple" } */

#include <stdlib.h>

struct S {
    int a;
    int b;
    int c;
};

__attribute__((noinline)) void __GIMPLE(ssa,startwith("struct_reorg")) test() {
  void* _1;

  __BB(2):
  _1 = calloc(1UL, 12UL);
  __MEM <int> ((int*)_1 + 4UL) = 0;
  __MEM <struct S> ((struct S*)_1).a = 0;

  return;
}

int main() {
    test();
    return 0;
}

/* { dg-final { scan-ipa-dump "struct S(\[0-9\]*) has escaped" "struct_reorg" } } */
