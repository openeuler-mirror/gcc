/* { dg-do compile { target aarch64-*-* } } */
/* { dg-options "-O3 -fconvert-minmax" } */

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
        /* This loop is not being vectorized now.  */
        for( int x = -2; x < width+3; x++ ) {
            int v = ((src)[x-2*stride] + (src)[x+3*stride] - 5*((src)[x-stride]
		     + (src)[x+2*stride]) + 20*((src)[x] + (src)[x+stride]));
            dstv[x] = clip ( (v + 16) >> 5 );
            buf[x+2] = v + pad;
        }

        /* Produces two versions of the code: 3xUZP1/2xMAX/2xMIN + 1xUZP1/1xMAX/1xMIN.  */
        for( int x = 0; x < width; x++ )
            dstc[x] = clip ((((buf+2)[x-2*1] + (buf+2)[x+3*1] - 5*((buf+2)[x-1]
			      + (buf+2)[x+2*1]) + 20*((buf+2)[x] + (buf+2)[x+1]))
			     - 32*pad + 512) >> 10);

        /* Priduces two versions of the code: 1xUZP1/2xMAX/2xMIN + 0xUZP1/1xMAX/1xMIN.  */
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

/* Max is performed on 0 from signed values, match smax exactly.  */
/* { dg-final { scan-assembler-times {smax\t} 6 } }  */
/* Min is performed on signed val>0 and a mask, min sign doesn't matter.  */
/* { dg-final { scan-assembler-times {[us]min\t} 6 } }  */
/* All of the vectorized patterns are expected to be matched.  */
/* { dg-final { scan-assembler-not {cmtst\t} } }  */
/* { dg-final { scan-assembler-times {uzp1\t} 5 } }  */
