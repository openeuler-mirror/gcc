/* { dg-do compile { target aarch64*-*-linux* } } */
/* { dg-options "-fno-prefetch-loop-arrays -O3 -fno-tree-vectorize -ftree-replacement-index-prefetch -fchecking=2 -fdump-tree-aprefetch-details" } */
extern void may_throw();
__attribute__((noipa)) unsigned
edge_exception(unsigned *q, const unsigned *t, unsigned next,
	       unsigned other, bool choose)
{
  unsigned v;
  try {
    if (choose) {v=*q; *q=next; may_throw();} else v=other;
  } catch (...) {return 0;}
  unsigned tail=0;
  for(unsigned k=0;k<other;k++) tail+=t[k&15];
  return t[v&255]+tail;
}

/* { dg-final { scan-tree-dump-not "Replacement-edge prefetch:" "aprefetch" } } */
