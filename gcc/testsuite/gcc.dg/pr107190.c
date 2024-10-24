/* { dg-do compile } */
/* { dg-options "-O2 -fexpensive-optimizations -fdump-tree-phiopt2-details" } */

#  define BN_BITS4        32
#  define BN_MASK2        (0xffffffffffffffffL)
#  define BN_MASK2l       (0xffffffffL)
#  define BN_MASK2h       (0xffffffff00000000L)
#  define BN_MASK2h1      (0xffffffff80000000L)
#  define LBITS(a)        ((a)&BN_MASK2l)
#  define HBITS(a)        (((a)>>BN_BITS4)&BN_MASK2l)
#  define L2HBITS(a)      (((a)<<BN_BITS4)&BN_MASK2)

unsigned int test_m(unsigned long in0, unsigned long in1) {
    unsigned long m, m1, lt, ht, bl, bh;
    lt = LBITS(in0);
    ht = HBITS(in0);
    bl = LBITS(in1);
    bh = HBITS(in1);
    m  = bh * lt;
    m1 = bl * ht;
    ht = bh * ht;
    m  = (m + m1) & BN_MASK2;
    if (m < m1) ht += L2HBITS((unsigned long)1);
    return ht + m;
}

/* { dg-final { scan-tree-dump "COND_EXPR in block 2 and PHI in block 4 converted to straightline code" "phiopt2" } } */
