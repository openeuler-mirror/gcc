/* { dg-do run } */
/* { dg-require-effective-target pthread } */
/* { dg-require-effective-target tls_runtime } */
/* { dg-options "-O3 -pthread -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

#include <pthread.h>

static __thread int tbl[16];
static __thread int ready[1];

static void __attribute__((noinline))
init (void)
{
  for (int i = 0; i < 16; ++i)
    tbl[i] = i * 3 + 7;
}

static int __attribute__((noinline))
get (int i)
{
  if (ready[0])
    return tbl[i];
  return -1;
}

static void *
worker (void *p)
{
  if (get (2) != -1)
    __builtin_abort ();
  init ();
  ready[0] = 1;
  if (get (2) != 13)
    __builtin_abort ();
  return 0;
}

int
main (void)
{
  init ();
  ready[0] = 1;
  pthread_t thread;
  if (pthread_create (&thread, 0, worker, 0)
      || pthread_join (thread, 0)
      || get (2) != 13)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump "array-load-inline: inlined load of tbl" "array-load-inline" } } */
