/* { dg-do run } */
/* { dg-options "-O2 -flto -ficp -fdump-ipa-icp=./icp3.c.077i.icp" } */

#include <stdio.h>

int dummy = 0;

typedef int (*ftype1)(int a);
typedef float (*ftype2)(int a);
typedef ftype1 (*ftype3) (ftype2);

ftype1 func1;

struct {
 int a;
 int* b;
 ftype1 myf1;
 ftype2 myf2;
 ftype3 myf3;
} my_str;

ftype1 boo(ftype2 a) {
  printf ("Call boo\n");
  return (ftype1) a;
}

int foo(int a) {
  printf ("Call foo\n");
  my_str.myf1 = func1;
  if (a % 2 == 0)
    dummy += dummy % (dummy - a);
  return a + 1;
}

float bar(int a) {
  printf("Call bar\n");
  my_str.myf2 = (ftype2) my_str.myf3((ftype2) foo);
  func1 = &foo;
  return foo(a);
}

int main() {
  my_str.myf3 = &boo;
  bar(1);
  my_str.myf2(3);
  return (my_str.myf1(2) + func1(4)) != 8;
}

/* { dg-final { scan-ipa-dump "The call is substituted by:.*= foo \\(4\\);" "icp" } } */
/* { dg-final { scan-ipa-dump "The call is substituted by:.*= foo \\(2\\);" "icp" } } */
/* { dg-final { scan-ipa-dump "The call is substituted by: foo \\(3\\);" "icp" } } */
/* { dg-final { scan-ipa-dump "STATS: 4 candidates for indirect call promotion, 3 substituted, 0 speculatively promoted, 0 cold" "icp" } } */
