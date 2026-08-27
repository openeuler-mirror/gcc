/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp1 -fno-tree-ccp -fno-tree-forwprop -fno-tree-fre" } */

void kill (void);

int f(int c){
  c |= 1;
  if (c == 0)
    kill ();

  return c;
}

/* { dg-final { scan-tree-dump-not "kill" "evrp1" } }  */
