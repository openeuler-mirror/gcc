/* { dg-do run } */
/* { dg-options "-O2 -flto -ficp -fdump-ipa-icp=./icp7.c.077i.icp" } */

#include <stdarg.h>

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

int boo(int a, ...) {
  va_list ap;
  va_start(ap, a);
  if (a == 0)
    dummy += va_arg(ap, int);
  va_end(ap);
  return dummy;
}

int foo(int a) {
  my_str.myf1 = func1;
  if (a % 2 == 0)
    dummy += dummy % (dummy - a);
  return a + 1;
}

float bar(int a) {
  my_str.myf2 = &bar;
  func1 = (ftype1) &boo;
  return foo(a);
}

int main() {
  bar(1);
  my_str.myf2(3);
  return (my_str.myf1(2) + func1(4));
}

/* { dg-final { scan-ipa-dump "Address taken function with varargs is found. Skip the optimization." "icp" } } */
