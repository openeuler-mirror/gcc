/* { dg-do compile } */
/* { dg-do run } */

#include <stdlib.h>
#include <stdio.h>

#ifdef STACK_SIZE
#if STACK_SIZE > 16000
#define N 1000
#else
#define N (STACK_SIZE/16)
#endif
#else
#define N 1000
#endif

int num;

int (*foo)(int d);
int f (int t);

typedef struct str_t str_t1;
struct str_t
{
   int a;
   float b;
   int (*foo)(int d);
};

int main ()
{
   int i, r;
   r = rand ();
   num = r > N ? N : r;
   str_t1 * p1 = calloc (num, sizeof (str_t1));
   if (p1 == NULL)
      return 0;
   for (i = 0; i < num; i++)
     {
       p1[i].foo = malloc (1 * sizeof (f));
       p1[i].foo = f;
       p1[i].foo (i);
     }

   for (i = 0; i < num; i++)
      p1[i].a = 1;

   for (i = 0; i < num; i++)
      p1[i].b = 2;

   for (i = 0; i < num; i++)
      if (p1[i].a != 1)
	 abort ();

   for (i = 0; i < num; i++)
      if (abs (p1[i].b - 2) > 0.0001)
	 abort ();

   return 0;
}

int f (int t)
{
   if ( t < 0)
      abort ();
   return 0;
}

/* { dg-final { scan-ipa-dump-times "Dead field elimination" 0 "struct_reorg" } } */
