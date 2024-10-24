/* { dg-do compile } */
/* { dg-options "-O2 -fmerge-mull -Wno-psabi -fdump-tree-forwprop1-details -fdump-tree-forwprop4-details" } */

#  define BN_BITS4        32
#  define BN_MASK2        (0xffffffffffffffffL)
#  define BN_MASK2l       (0xffffffffL)
#  define BN_MASK2h       (0xffffffff00000000L)
#  define BN_MASK2h1      (0xffffffff80000000L)
#  define LBITS(a)        ((a)&BN_MASK2l)
#  define HBITS(a)        (((a)>>BN_BITS4)&BN_MASK2l)
#  define L2HBITS(a)      (((a)<<BN_BITS4)&BN_MASK2)

void mul64(unsigned long in0, unsigned long in1,
           unsigned long &retLo, unsigned long &retHi) {
    unsigned long m00, m01, m10, m11, al, ah, bl, bh;
    unsigned long Addc, addc32, low;
    al = LBITS(in0);
    ah = HBITS(in0);
    bl = LBITS(in1);
    bh = HBITS(in1);
    m10 = bh * al;
    m00 = bl * al;
    m01 = bl * ah;
    m11 = bh * ah;
    Addc = (m10 + m01) & BN_MASK2;
    if (Addc < m01) m11 += L2HBITS((unsigned long)1);
    m11 += HBITS(Addc);
    addc32 = L2HBITS(Addc);
    low = (m00 + addc32) & BN_MASK2; if (low < addc32) m11++;
    retLo = low;
    retHi  = m11;
}

/* { dg-final { scan-tree-dump "gimple_simplified to" "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to" 1 "forwprop4" } } */
