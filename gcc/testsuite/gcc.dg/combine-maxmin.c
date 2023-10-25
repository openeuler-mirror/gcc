/* { dg-do compile { target aarch64-*-* } } */
/* { dg-options "-O3 -fdump-rtl-combine-all" } */

/* The test checks usage of smax/smin insns for clip evaluation and
 * uzp1/uzp2 insns for vector element narrowing.  It's inspired by
 * sources of x264 codec.  */

typedef unsigned char uint8_t;
typedef long int intptr_t;
typedef signed short int int16_t;

static __attribute__((always_inline)) inline uint8_t clip (int x )
{
    return ( (x & ~((1 << 8)-1)) ? (-x)>>31 & ((1 << 8)-1) : x );
}

void hf (uint8_t *dsth, uint8_t *dstv, uint8_t *dstc, uint8_t *src,
	 intptr_t stride, int width, int height, int16_t *buf)
{
    const int pad = (8 > 9) ? (-10 * ((1 << 8)-1)) : 0;
    for( int y = 0; y < height; y++ ) {
        for( int x = -2; x < width+3; x++ ) {
            int v = ((src)[x-2*stride] + (src)[x+3*stride] - 5*((src)[x-stride]
		     + (src)[x+2*stride]) + 20*((src)[x] + (src)[x+stride]));
            dstv[x] = clip ( (v + 16) >> 5 );
            buf[x+2] = v + pad;
        }
        for( int x = 0; x < width; x++ )
            dstc[x] = clip ((((buf+2)[x-2*1] + (buf+2)[x+3*1] - 5*((buf+2)[x-1]
			      + (buf+2)[x+2*1]) + 20*((buf+2)[x] + (buf+2)[x+1]))
			     - 32*pad + 512) >> 10);
        for( int x = 0; x < width; x++ )
            dsth[x] = clip ((((src)[x-2*1] + (src)[x+3*1] - 5*((src)[x-1]
			      + (src)[x+2*1]) + 20*((src)[x] + (src)[x+1]))
			     + 16) >> 5);
        dsth += stride;
        dstv += stride;
        dstc += stride;
        src += stride;
    }
}

/* { dg-final { scan-assembler-times {smax\t} 4 } }  */
/* { dg-final { scan-assembler-times {smin\t} 4 } }  */
/* { dg-final { scan-assembler-times {cmtst\t} 2 } }  */
/* { dg-final { scan-assembler-times {uzp1\t} 6 } }  */
