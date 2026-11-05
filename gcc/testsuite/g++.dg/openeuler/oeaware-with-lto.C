/* { dg-do compile { target *-*-linux* *-*-gnu* } }  */
/* { dg-options "-foeaware-policy=1 -flto -ffat-lto-objects" }  */

namespace radar8446940 {
int main () {
 return 0;
}
}

int main () {
  return 0;
}