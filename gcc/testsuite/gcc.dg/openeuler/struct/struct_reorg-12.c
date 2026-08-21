/* { dg-do compile } */
/* { dg-options "-w -g -O3 -flto-partition=one -fipa-struct-reorg -fwhole-program -S" } */

struct foo {
  long element1;
  long element2;
};

struct goo {
  struct foo element_foo;
};

struct goo g1;

void func () {
  struct foo (*local)[] = 0;
  long idx;
  (g1).element_foo = (*local)[idx];
}

struct foo g2;
int main () {
  func ();
  g2 = g1.element_foo;
  return 0;
}
