/* { dg-do compile } */

#include <stdio.h>
#include <stdlib.h>

#define MallocOrDie(x)     sre_malloc((x))

struct gki_elem {
  char            *key;
  int              idx;
  struct gki_elem *nxt;
};

typedef struct {
  struct gki_elem **table;

  int primelevel;
  int nhash;
  int nkeys;
} GKI;

void
Die(char *format, ...)
{
  exit(1);
}

void *
sre_malloc(size_t size)
{
  void *ptr;

  if ((ptr = malloc (size)) == NULL)
    {
      Die("malloc of %ld bytes failed", size);
    }
  return ptr;
}


__attribute__((noinline)) int
GKIStoreKey(GKI *hash, char *key)
{
  hash->table[0] = MallocOrDie(sizeof(struct gki_elem));
}

int
main ()
{
  GKI *hash;
  char *key;
  GKIStoreKey(hash, key);
  return 0;
}

/* { dg-final { scan-ipa-dump "Number of structures to transform is 1" "reorder_fields" } } */