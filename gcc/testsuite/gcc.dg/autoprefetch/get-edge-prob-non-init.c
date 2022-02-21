/* { dg-do compile } */
/* { dg-options "-Ofast -fprefetch-loop-arrays=2 -fdump-tree-aprefetch-details" } */

int a, c, f;
static int *b = &a;
int *d;
int e[0];
void g() {
  int h;
  for (;;) {
    h = 1;
    for (; h >= 0; h--) {
      c = 2;
      for (; c; c--)
        if (e[0])
          if (e[c])
            *b = 0;
      f || (*d = 0);
    }
  }
}
int main() {}

/* { dg-final } */
