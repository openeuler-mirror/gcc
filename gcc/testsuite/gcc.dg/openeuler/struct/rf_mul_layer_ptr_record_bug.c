/* { dg-do compile } */

#include <stdio.h>
#include <stdlib.h>

typedef struct T_HASH_ENTRY
{ 
  unsigned int hash;
  unsigned int klen;
  char *key;
} iHashEntry;

typedef struct T_HASH
{
  unsigned int size;
  unsigned int fill;
  unsigned int keys;

  iHashEntry **array;
} uHash;

uHash *retval;

int
main() {
  retval->array = (iHashEntry **)calloc(sizeof(iHashEntry *), retval->size);
  return 0;
}

/* { dg-final { scan-ipa-dump "Number of structures to transform is 2" "struct_reorg" } } */