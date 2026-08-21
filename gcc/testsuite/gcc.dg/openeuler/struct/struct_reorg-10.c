/* { dg-do compile } */
/* { dg-options "-w -g -O3 -flto-partition=one -fipa-struct-reorg -fwhole-program -S" } */

struct a {
  int b;
  char c;
};
struct {
  double d;
  _Bool e;
} * f;
struct g {
  struct a h;
} i;
long j;
void k();
void l() { k(i); }
void k(struct a m) {
  f->e = 0;
  for (;;)
    l();
}
int main() {
  for (; j; f = 0) {
    struct g *n = 0;
    char o = n->h.c;
  }
  l();
}
