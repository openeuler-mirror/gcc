/* { dg-do run } */
/* { dg-options "-O2 -flto -ficp -fdump-ipa-icp=./icp6.c.077i.icp -Wno-int-conversion -Wno-incompatible-pointer-types" } */
int dummy = 0;

typedef int (*ftype1)(int a);
typedef float (*ftype2)(int a);
typedef int (*ftype3)();
typedef int (*ftype4)(int a, int b);

ftype1 func1;
ftype4 func2;

struct {
 int a;
 int* b;
 ftype1 myf1;
 ftype2 myf2;
 ftype3 myf3;
} my_str;

int foo3(float a) {
  return dummy;
}

int foo4(int a, int b) {
  return a*b;
}

int foo(int a) {
  my_str.myf1 = func1;
  if (a % 2 == 0)
    dummy += dummy % (dummy - a);
  return a + 1;
}

int foo2(float a) {
 func1 = (ftype1) &foo;
 func2 = &foo4;
 return dummy + foo3 (a);
}

float bar2(int a) {
  my_str.myf2 = (ftype2)(0x864213);
  func2 = 0x65378;
  return foo(a);
}

float bar(int a) {
  my_str.myf3 = &foo2;
  my_str.myf2 = &bar;
  func1 = (ftype1) &dummy;
  func2 = (ftype4) &bar2;
  return foo(a);
}

int main() {
  bar(1);
  bar2(1);
  bar(0);
  my_str.myf2(3);
  ((ftype1) my_str.myf3)(0.0);
  int sum = func1(4);
  return (sum + my_str.myf1(2) + func2(5, 6)) != 38;
}
/* { dg-final { scan-ipa-dump "The call is substituted by.*foo2 \\(0\\);" "icp" } } */
/* { dg-final { scan-ipa-dump "STATS: 5 candidates for indirect call promotion, 1 substituted, 0 speculatively promoted, 0 cold" "icp" } } */
