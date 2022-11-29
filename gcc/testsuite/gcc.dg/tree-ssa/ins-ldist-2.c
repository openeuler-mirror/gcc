/* { dg-do compile { target { aarch64*-*-linux* } } } */
/* { dg-options "-O3 -ftree-slp-transpose-vectorize -fdump-tree-ldist-all-details" } */

unsigned a0[4], a1[4], a2[4], a3[4];

void foo (unsigned char *oxa, int ia, unsigned char *oxb, int ib)
{
  for (int i = 0; i < 4; i++, oxa += ia, oxb += ib)
    {
      a0[i] = (oxa[0] - oxb[0]) + ((oxa[4] - oxb[4]) << 16);
      a1[i] = (oxa[1] - oxb[1]) + ((oxa[5] - oxb[5]) << 16);
      a2[i] = (oxa[2] - oxb[2]) + ((oxa[6] - oxb[6]) << 16);
      a3[i] = (oxa[3] - oxb[3]) + ((oxa[7] - oxb[7]) << 16);
    }
}

/* { dg-final { scan-tree-dump-times "Loop 1 not distributed." 1 "ldist" } } */
