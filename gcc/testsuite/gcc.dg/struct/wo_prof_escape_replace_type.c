/* { dg-do compile } */

#include <stdlib.h>

struct AngleDef
{
  double K;
  double th0;
};
typedef struct AngleDef angldef;

struct bndangdihe
{
  int nbond;
  int nangl;
  int ndihe;
};
typedef struct bndangdihe bah;

struct ambprmtop
{
  double *AnglK;
  double *AnglEq;
  bah nBAH;
  angldef *AParam;
  char source[512];
  char eprulesource[512];
};
typedef struct ambprmtop prmtop;

static void OrderBondParameters (prmtop *tp)
{
  int i;
  tp->AParam = (angldef *)malloc (tp->nBAH.nangl * sizeof (angldef));
  for (i = 0; i < tp->nBAH.nangl; i++)
    {
      tp->AParam[i].K = tp->AnglK[i];
      tp->AParam[i].th0 = tp->AnglEq[i];
    }
}

void main ()
{
  prmtop *tp = (prmtop *)malloc (100 * sizeof (prmtop));
  OrderBondParameters (tp);
}

/*---------------------------------------------------------------------------------------------*/
/* { dg-final { scan-ipa-dump "No structures to transform in struct split." "struct_reorg" } } */
