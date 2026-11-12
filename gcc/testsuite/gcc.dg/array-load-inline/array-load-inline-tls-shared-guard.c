/* { dg-do run } */
/* { dg-require-effective-target pthread } */
/* { dg-require-effective-target tls_runtime } */
/* { dg-options "-O3 -pthread -fipa-array-load-inline -fdump-ipa-array-load-inline-details" } */

#include <pthread.h>

static __thread int tbl[16];
static int ready[1];

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
  /* The shared guard is set, but this thread's table is still zero.  */
  if (get (2) != 0)
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

/* { dg-final { scan-ipa-dump-not "array-load-inline: inlined load of tbl" "array-load-inline" } } */
