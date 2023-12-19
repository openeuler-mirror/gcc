/* { dg-do compile } */
/* { dg-options "-w -g -O3 -flto-partition=one -fipa-struct-reorg -fwhole-program -S" } */

struct a {
  int b;
  double c;
};
struct d {
  struct a e;
};
int f;
int main() {
  _Bool g;
  struct d **h = 0;
  g = *h += f;
}
