// Structures without names should not be optimized
/* { dg-do compile } */
#include <stdlib.h>
#include <math.h>

typedef struct
{
  int a;
  float b;
  double s1;
  double s2;
  double s3;
  double s4;
  double s5;
  double s6;
  double s7;
  double s8;
} str_t1;

#define N 1000

int num;

int
main ()
{
  int i, r;

  r = rand ();
  num = r > N ? N : r;
  str_t1 *p1 = calloc (num, sizeof (str_t1));

  if (p1 == NULL)
    return 0;

  for (i = 0; i < num; i++)
    p1[i].a = 1;

  for (i = 0; i < num; i++)
    p1[i].b = 2;

  for (i = 0; i < num; i++)
    if (p1[i].a != 1)
      abort ();

  for (i = 0; i < num; i++)
    if (fabsf (p1[i].b - 2) > 0.0001)
      abort ();

  return 0;
}

/* { dg-final { scan-ipa-dump "No structures to transform in pointer compression" "struct_reorg" } } */