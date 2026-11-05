/* { dg-do run } */
/* { dg-options "-O2 -flto -ficp -fdump-ipa-icp=./icp2.c.077i.icp" } */

int dummy = 0;

typedef int (*ftype1)(int a);
typedef float (*ftype2)(int a);

ftype1 func1;

struct {
 int a;
 int* b;
 ftype1 myf1;
 ftype2 myf2;
} my_str;

int foo(int a) {
  my_str.myf1 = func1;
  if (a % 2 == 0)
    dummy += dummy % (dummy - a);
  return a + 1;
}

float bar(int a) {
  my_str.myf2 = dummy ? (ftype2) &foo : &bar;
  func1 = (ftype1) &bar;
  return foo(a);
}

int main() {
  bar(1);
  my_str.myf2(3);
  return (my_str.myf1(2) + func1(4)) != 8;
}

/* { dg-final { scan-ipa-dump-not "The call is substituted by.*" "icp" } } */
/* { dg-final { scan-ipa-dump "STATS: 3 candidates for indirect call promotion, 0 substituted, 0 speculatively promoted, 0 cold" "icp" } } */
